#include "jit.h"
#include "vm.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include "jit_codegen.h"

#ifdef _WIN32
# include <windows.h>
#else
# include <unistd.h>
# include <sys/mman.h>
#endif

VM* jit_vm = NULL;
extern const uint8_t opcode_operand_length[256];

static void bc_remove_bytes(Bytecode* bc, uint32_t func_idx, uint32_t offset, size_t remove_size) {
    if (remove_size == 0) return;

    Function* func = &bc->functions[func_idx];

    uint32_t old_func_end = func->code_end;

    memmove(&bc->code[offset],
            &bc->code[offset + remove_size],
            bc->code_size - (offset + remove_size));

    if (bc->line_numbers) {
        memmove(&bc->line_numbers[offset],
                &bc->line_numbers[offset + remove_size],
                (bc->code_size - (offset + remove_size)) * sizeof(int));
    }

    bc->code_size -= remove_size;

    func->code_end -= (uint32_t)remove_size;

    for (uint32_t f = 0; f < bc->func_count; f++) {
        if (bc->functions[f].code_start >= old_func_end && f != func_idx) {
            bc->functions[f].code_start -= (uint32_t)remove_size;
            bc->functions[f].code_end -= (uint32_t)remove_size;
        }
    }

    if (jit_vm) {
        for (int f = 0; f < jit_vm->frame_count; f++) {
            CallFrame* frame = &jit_vm->frames[f];
            if (frame->bytecode == bc &&
                frame->ip > (bc->code + offset)) {
                frame->ip -= (uint8_t)remove_size;
                }
        }
    }
}

static void bc_fix_jumps(Bytecode* bc, uint32_t func_idx, int debug) {
    Function* func = &bc->functions[func_idx];

    for (uint32_t i = func->code_start; i < func->code_end; ) {
        if (i >= func->code_end) break;

        uint8_t op = bc->code[i];
        uint8_t oplen = opcode_operand_length[op];

        if (i + 1 + oplen > func->code_end) break;

        if (op == OP_JUMP_U16 || op == OP_JUMP_IF_FALSE_U16 || op == OP_JUMP_IF_TRUE_U16) {
            if (oplen != 2) {
                i += 1 + oplen;
                continue;
            }

            int16_t old_offset = (int16_t)((bc->code[i + 1] << 8) | bc->code[i + 2]);

            uint32_t instr_end = i + 3;
            int32_t target_raw = (int32_t)instr_end + (int32_t)old_offset;

            if (target_raw >= (int32_t)func->code_start && target_raw < (int32_t)func->code_end) {
                uint32_t target = (uint32_t)target_raw;

                int32_t new_offset_raw = (int32_t)target - (int32_t)(i + 3);

                if (new_offset_raw < INT16_MIN || new_offset_raw > INT16_MAX) {
                    if (debug) {
                        printf("[JIT ERROR] Jump offset out of range after fix: %d (target=%u, instr=%u)\n",
                               (int)new_offset_raw, target, i);
                    }
                    new_offset_raw = new_offset_raw > 0 ? INT16_MAX : INT16_MIN;
                }

                int16_t new_offset = (int16_t)new_offset_raw;
                bc->code[i + 1] = (uint8_t)((new_offset >> 8) & 0xFF);
                bc->code[i + 2] = (uint8_t)(new_offset & 0xFF);

                if (debug && old_offset != new_offset) {
                    printf("[JIT Tier-1 FIX-JUMP] Func %u Instr @%u: offset %d->%d (target=%u)\n",
                           func_idx, i, old_offset, new_offset, target);
                }
            } else if (debug && target_raw != INT32_MIN) {
                printf("[JIT WARNING] Jump @%u has target %d outside func [%u-%u]\n",
                       i, target_raw, func->code_start, func->code_end);
            }

            i += 1 + oplen;
        } else {
            i += 1 + oplen;
        }
    }
}

JIT* jit_new(void) {
    JIT* jit = (JIT*)malloc(sizeof(JIT));
    if (!jit) return NULL;

    jit->enabled = 1;
    jit->debug = 0;
    jit->optimize_threshold = 10;
    jit->native_threshold = 50;
    jit->opt_flags = JIT_OPT_ALL;
    jit->compiled_funcs = NULL;
    jit->compiled_count = 0;
    jit->compiled_capacity = 0;
    jit->hotness_counters = NULL;
    jit->counter_count = 0;
    jit->total_calls = 0;
    jit->total_optimized = 0;
    jit->total_native = 0;

    return jit;
}

static void jit_free_executable(void* mem, size_t size) {
    (void)size;
    if (!mem) return;
#ifdef _WIN32
    VirtualFree(mem, 0, MEM_RELEASE);
#else
    munmap(mem, size);
#endif
}

void jit_free(JIT* jit) {
    if (!jit) return;

    for (size_t i = 0; i < jit->compiled_count; i++) {
        if (jit->compiled_funcs[i].tier == TIER_NATIVE &&
            jit->compiled_funcs[i].native_code &&
            jit->compiled_funcs[i].code_size > 0) {
            jit_free_executable(
                (void*)(uintptr_t)jit->compiled_funcs[i].native_code,
                jit->compiled_funcs[i].code_size
            );
        }
    }

    free(jit->compiled_funcs);
    free(jit->hotness_counters);
    free(jit);
}

void jit_increment_hotness(JIT* jit, uint32_t func_idx) {
    if (!jit || !jit->enabled) return;

    if (func_idx >= jit->counter_count) {
        size_t new_count = (size_t)func_idx + 1;
        uint64_t* new_counters =
            (uint64_t*)realloc(jit->hotness_counters,
                               new_count * sizeof(uint64_t));
        if (!new_counters) return;

        memset(new_counters + jit->counter_count, 0,
               (new_count - jit->counter_count) * sizeof(uint64_t));

        jit->hotness_counters = new_counters;
        jit->counter_count = new_count;
    }

    jit->hotness_counters[func_idx]++;
    jit->total_calls++;
}

CompilationTier jit_get_target_tier(JIT* jit, uint32_t func_idx) {
    if (!jit || !jit->enabled || func_idx >= jit->counter_count) {
        return TIER_INTERPRETER;
    }

    uint64_t hotness = jit->hotness_counters[func_idx];

    if (hotness >= jit->native_threshold) {
        return TIER_NATIVE;
    } else if (hotness >= jit->optimize_threshold) {
        return TIER_OPTIMIZED_BC;
    }

    return TIER_INTERPRETER;
}

CompiledFunction* jit_get_compiled_func(JIT* jit, uint32_t func_idx) {
    if (!jit) return NULL;

    for (size_t i = 0; i < jit->compiled_count; i++) {
        if (jit->compiled_funcs[i].func_idx == func_idx) {
            return &jit->compiled_funcs[i];
        }
    }

    return NULL;
}

int jit_should_compile(JIT* jit, uint32_t func_idx) {
    if (!jit || !jit->enabled) return 0;

    CompilationTier target = jit_get_target_tier(jit, func_idx);
    CompiledFunction* exist = jit_get_compiled_func(jit, func_idx);

    if (!exist) {
        return target > TIER_INTERPRETER;
    }

    return target > exist->tier;
}

int jit_bc_constant_folding(Bytecode* bc, uint32_t func_idx, int debug) {
    int folded = 0;
    Function* func = &bc->functions[func_idx];

    printf("[JIT Tier-1 OPT-CF] Folding func %u '%s' [%u..%u) size=%u\n",
           func_idx, func->name ? func->name : "<anon>",
           func->code_start, func->code_end, func->code_end - func->code_start);

    uint32_t i = func->code_start;
    int patterns_found = 0;

    while (i < func->code_end) {
        if (i + 3 > func->code_end) {
            if (debug) printf("[JIT Tier-1 OPT-CF END] Reached end of func @%u\n", i);
            break;
        }

        uint8_t op1 = bc->code[i];

        if (op1 != OP_LOAD_CONST_U16) {
            uint8_t oplen = opcode_operand_length[op1];
            i += 1 + oplen;
            continue;
        }

        uint16_t idx1 = (uint16_t)((bc->code[i + 1] << 8) | bc->code[i + 2]);

        if (i + 3 > func->code_end || idx1 >= bc->const_count) {
            i += 3;
            continue;
        }

        uint8_t op2 = bc->code[i + 3];

        if (op2 != OP_LOAD_CONST_U16) {
            i += 3;
            continue;
        }

        if (i + 6 > func->code_end) {
            i += 3;
            continue;
        }

        uint16_t idx2 = (uint16_t)((bc->code[i + 4] << 8) | bc->code[i + 5]);

        if (idx2 >= bc->const_count) {
            i += 3;
            continue;
        }

        if (i + 7 > func->code_end) {
            i += 3;
            continue;
        }

        uint8_t op3 = bc->code[i + 6];

        patterns_found++;
        if (debug) {
            printf("[JIT Tier-1 OPT-CF PATTERN FOUND] @%u: LOAD_CONST[%u]=%lld; LOAD_CONST[%u]=%lld; %s\n",
                   i, idx1, bc->constants[idx1].int_val,
                   idx2, bc->constants[idx2].int_val,
                   op3 == OP_ADD ? "ADD" :
                   op3 == OP_SUB ? "SUB" :
                   op3 == OP_MUL ? "MUL" :
                   op3 == OP_DIV ? "DIV" : "MOD");
        }

        if (bc->constants[idx1].type != CONST_INT ||
            bc->constants[idx2].type != CONST_INT) {
            i += 3;
            continue;
        }

        int64_t val1 = bc->constants[idx1].int_val;
        int64_t val2 = bc->constants[idx2].int_val;
        int64_t result = 0;
        int can_fold = 0;

        switch (op3) {
            case OP_ADD:
                result = val1 + val2;
                can_fold = 1;
                break;
            case OP_SUB:
                result = val1 - val2;
                can_fold = 1;
                break;
            case OP_MUL:
                result = val1 * val2;
                can_fold = 1;
                break;
            case OP_DIV:
                if (val2 != 0) {
                    result = val1 / val2;
                    can_fold = 1;
                }
                break;
            case OP_MOD:
                if (val2 != 0) {
                    result = val1 % val2;
                    can_fold = 1;
                }
                break;
            default:
                can_fold = 0;
        }

        if (!can_fold) {
            if (debug) printf("[JIT Tier-1 OPT-CF SKIP] Cannot fold op %u\n", op3);
            i += 3;
            continue;
        }

        uint16_t result_idx = bc->const_count;

        if (debug) {
            printf("[JIT Tier-1 OPT-CF FOLD] %lld %s %lld = %lld → new const #%u\n",
                   val1, op3 == OP_ADD ? "+" : "-", val2, result, result_idx);
        }

        if (bc->const_count >= bc->const_capacity) {
            size_t new_cap = bc->const_capacity == 0 ? 16 : bc->const_capacity * 2;
            Constant* new_consts = (Constant*)realloc(bc->constants,
                                                      new_cap * sizeof(Constant));
            if (!new_consts) {
                if (debug) printf("[JIT Tier-1 OPT-CF FAIL] Cannot allocate new const");
                i += 3;
                continue;
            }

            bc->constants = new_consts;
            bc->const_capacity = new_cap;
        }

        bc->constants[bc->const_count].type = CONST_INT;
        bc->constants[bc->const_count].int_val = result;
        bc->const_count++;

        bc->code[i] = OP_LOAD_CONST_U16;
        bc->code[i + 1] = (uint8_t)((result_idx >> 8) & 0xFF);
        bc->code[i + 2] = (uint8_t)(result_idx & 0xFF);

        bc_remove_bytes(bc, func_idx, i + 3, 4);

        bc_fix_jumps(bc, func_idx, 0);

        folded++;

        if (debug) printf("[JIT Tier-1 OPT-CF SUCCESS] Now folded: %d\n", folded);
    }

    return folded;
}

int jit_optimize_bytecode(JIT* jit, Bytecode* bc, uint32_t func_idx) {
    if (!jit || !bc || func_idx >= bc->func_count) return 0;

    int total_opts = 0;
    int passes = 0;
    const int max_passes = 3;

    //bc_disassemble(bc, "pre-opt");

    while (passes < max_passes) {
        int pass_opts = 0;

        pass_opts += jit_bc_constant_folding(bc, func_idx, jit->debug);

        total_opts += pass_opts;
        passes++;

        //bc_disassemble(bc, "opt");

        if (pass_opts == 0) break;
    }

    bc_fix_jumps(bc, func_idx, jit->debug);

    if (jit->debug && total_opts > 0) {
        printf("[JIT Tier-1 OPT] Function %u: %d optimizations in %d passes\n",
               func_idx, total_opts, passes);
    }

    return total_opts;
}

static CompiledFunction* ensure_compiled_entry(JIT* jit, uint32_t func_idx) {
    CompiledFunction* cf = jit_get_compiled_func(jit, func_idx);
    if (cf) return cf;

    if (jit->compiled_count >= jit->compiled_capacity) {
        size_t old = jit->compiled_capacity;
        size_t newcap = old == 0 ? 8 : old * 2;
        CompiledFunction* nf =
            (CompiledFunction*)realloc(jit->compiled_funcs,
                                       newcap * sizeof(CompiledFunction));
        if (!nf) return NULL;

        jit->compiled_funcs = nf;
        jit->compiled_capacity = newcap;
    }

    cf = &jit->compiled_funcs[jit->compiled_count++];
    memset(cf, 0, sizeof(*cf));
    cf->func_idx = func_idx;
    cf->tier = TIER_INTERPRETER;
    cf->native_code = NULL;
    cf->code_size = 0;
    cf->promotion_count = 0;
    cf->native_failed = 0;

    return cf;
}

void* jit_compile_or_promote(JIT* jit, Bytecode* bc, uint32_t func_idx) {
    if (!jit || !bc || func_idx >= bc->func_count) return NULL;

    CompilationTier target = jit_get_target_tier(jit, func_idx);
    CompiledFunction* cf = jit_get_compiled_func(jit, func_idx);
    Function* func = &bc->functions[func_idx];

    if (target == TIER_OPTIMIZED_BC &&
        (!cf || cf->tier < TIER_OPTIMIZED_BC)) {
        if (jit->debug) {
            printf("[JIT Tier-1] Optimizing bytecode for '%s' (hotness=%llu)\n",
                   func->name ? func->name : "",
                   (unsigned long long)jit->hotness_counters[func_idx]);
        }

        int optimizations = jit_optimize_bytecode(jit, bc, func_idx);

        if (jit->debug) {
            printf("[JIT Tier-1] Applied %d optimizations\n", optimizations);
        }

        cf = ensure_compiled_entry(jit, func_idx);
        if (!cf) return NULL;

        cf->tier = TIER_OPTIMIZED_BC;
        cf->optimized_bc.original_code_start = func->code_start;
        cf->optimized_bc.original_code_end = func->code_end;
        cf->code_size = func->code_end - func->code_start;
        cf->promotion_count = jit->hotness_counters[func_idx];

        jit->total_optimized++;

        return NULL;
    }

    return NULL;

    if (target == TIER_NATIVE &&
        (!cf || cf->tier < TIER_NATIVE)) {
        cf = ensure_compiled_entry(jit, func_idx);
        if (!cf) return NULL;

        if (cf->native_failed) {
            if (jit->debug >= 2) {
                printf("[JIT Tier-2] Skipping native compilation for '%s' (previous attempt failed)\n",
                       func->name ? func->name : "");
            }

            return NULL;
        }

        if (jit->debug) {
            printf("[JIT Tier-2] Compiling to native code for '%s' (hotness=%llu)\n",
                   func->name ? func->name : "",
                   (unsigned long long)jit->hotness_counters[func_idx]);
        }

        JitCodegen* cg = jit_codegen_new();
        if (!cg) {
            cf->native_failed = 1;
            return NULL;
        }

        JitNativeFunc native_fn = jit_compile_function(cg, bc, func_idx, jit->debug);
        if (!native_fn) {
            if (jit->debug) {
                printf("[JIT Tier-2] Failed to compile function %u\n", func_idx);
            }

            cf->native_failed = 1;
            jit_codegen_free(cg);
            return NULL;
        }

        cf->tier = TIER_NATIVE;
        cf->native_code = native_fn;
        cf->code_size = jit_codegen_get_size(cg);
        cf->promotion_count = jit->hotness_counters[func_idx];

        jit->total_native++;

        if (jit->debug) {
            printf("[JIT Tier-2] Generated %zu bytes at %p\n",
                   cf->code_size, (void*)native_fn);
        }

        jit_codegen_free(cg);

        return (void*)native_fn;
    }

    return NULL;
}

int jit_is_native(JIT* jit, uint32_t func_idx) {
    CompiledFunction* cf = jit_get_compiled_func(jit, func_idx);
    return cf && cf->tier == TIER_NATIVE && cf->native_code != NULL;
}

JitNativeFunc jit_get_native_code(JIT* jit, uint32_t func_idx) {
    CompiledFunction* cf = jit_get_compiled_func(jit, func_idx);
    if (cf && cf->tier == TIER_NATIVE) {
        return cf->native_code;
    }

    return NULL;
}

void jit_print_stats(JIT* jit) {
    if (!jit) return;

    printf("[JIT Statistics]\n");
    printf(" Total calls: %llu\n", (unsigned long long)jit->total_calls);
    printf(" Tier 1 (optimized): %llu\n", (unsigned long long)jit->total_optimized);
    printf(" Tier 2 (native): %llu\n", (unsigned long long)jit->total_native);
    printf(" Compiled functions: %zu\n", jit->compiled_count);
}

void jit_set_debug(JIT* jit, int level) {
    if (jit) jit->debug = level;
}

void jit_set_thresholds(JIT* jit, uint64_t opt_threshold, uint64_t native_threshold) {
    if (!jit) return;

    jit->optimize_threshold = opt_threshold;
    jit->native_threshold = native_threshold;
}