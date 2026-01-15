#include "jit_vm_bridge.h"
#include <stdio.h>
#include "jit_codegen.h"

void jit_vm_init(VM* vm, int debug,
                 uint64_t opt_threshold,
                 uint64_t native_threshold,
                 int opt_level)
{
    if (!vm) return;

    vm->jit = jit_new();
    if (!vm->jit) return;

    vm->jit->enabled           = 1;
    vm->jit->debug             = debug;
    vm->jit->optimize_threshold = opt_threshold;
    vm->jit->native_threshold   = native_threshold;

    if (opt_level == 0) {
        vm->jit->opt_flags = JIT_OPT_NONE;
    } else if (opt_level == 1) {
        vm->jit->opt_flags = JIT_OPT_CONSTANT_FOLD | JIT_OPT_PEEPHOLE;
    } else {
        vm->jit->opt_flags = JIT_OPT_ALL;
    }

    if (debug) {
        printf("[JIT] Initialized (opt@%llu, native@%llu, level=O%d)\n",
               (unsigned long long)opt_threshold,
               (unsigned long long)native_threshold,
               opt_level);
    }
}

void jit_vm_cleanup(VM* vm) {
    if (!vm || !vm->jit) return;
    jit_free(vm->jit);
    vm->jit = NULL;
}

typedef int (*JitNativeFn)(VM* vm);

bool jit_vm_handle_call(VM* vm, uint32_t func_idx, Bytecode* bytecode) {
    if (!vm || !vm->jit || !vm->jit->enabled) {
        return false;
    }

    vm->jit->total_calls++;

    jit_increment_hotness(vm->jit, func_idx);

    CompiledFunction* cf = jit_get_compiled_func(vm->jit, func_idx);

    if (vm->jit->debug && cf) {
        static int debug_counter = 0;
        if (debug_counter++ < 5 || debug_counter % 1000 == 0) {
            /*printf("[JIT-CALL #%d] func=%u tier=%d native=%p hotness=%llu\n",
                    debug_counter,
                    func_idx,
                    cf->tier,
                    cf->native_code,
                    func_idx < vm->jit->counter_count ?
                        (unsigned long long)vm->jit->hotness_counters[func_idx] : 0ULL);*/
        }
    }

    if (cf && cf->tier == TIER_NATIVE && cf->native_code) {
        if (vm->jit->debug) {
            static int exec_counter = 0;
            if (exec_counter++ < 5) {
                //printf("[JIT-EXEC] Executing native code for func %u\n", func_idx);
            }
        }
        vm->jit->total_native_executed++;
        JitNativeFn fn = (JitNativeFn)(uintptr_t)cf->native_code;
        int rc = fn(vm);
        return rc == 0;
    }

    if (!jit_should_compile(vm->jit, func_idx)) {
        vm->jit->total_interp_executed++;
        return false;
    }

    // DEBUG: Начало компиляции
    if (vm->jit->debug) {
        /*printf("[JIT-COMPILE-START] func=%u current_tier=%d hotness=%llu\n",
                func_idx,
                cf ? cf->tier : -1,
                func_idx < vm->jit->counter_count ?
                    (unsigned long long)vm->jit->hotness_counters[func_idx] : 0ULL);*/
    }

    void* result = jit_compile_or_promote(vm->jit, bytecode, func_idx);

    if (vm->jit->debug) {
        //printf("[JIT-COMPILE-RESULT] func=%u result=%p\n", func_idx, result);
    }

    if (!result) {
        if (vm->jit->debug) {
            //printf("[JIT-COMPILE-FAIL] func=%u compilation returned NULL\n", func_idx);
        }
        vm->jit->total_interp_executed++;
        return false;
    }

    if (vm->jit->debug) {
        //printf("[JIT] Compiled native for func %u\n", func_idx);
    }

    // Re-fetch после компиляции
    cf = jit_get_compiled_func(vm->jit, func_idx);

    // DEBUG: Состояние после компиляции
    if (vm->jit->debug) {
        /*printf("[JIT-AFTER-COMPILE] cf=%p tier=%d native=%p\n",
                (void*)cf,
                cf ? cf->tier : -1,
                cf ? cf->native_code : NULL);*/
    }

    if (!cf || cf->tier != TIER_NATIVE || !cf->native_code) {
        if (vm->jit->debug) {
            //printf("[JIT-ERROR] Cannot execute: cf=%p ", (void*)cf);
            if (cf) {
                /*printf("tier=%d (expected %d) native=%p\n",
                      cf->tier, TIER_NATIVE, cf->native_code);*/
            } else {
                //printf("(cf is NULL)\n");
            }
        }
        vm->jit->total_interp_executed++;
        return false;
    }

    if (vm->jit->debug) {
        printf("[JIT-FIRST-EXEC] func=%u native=%p\n", func_idx, cf->native_code);
    }

    /*printf("[BEFORE-JIT] vm=%p stack=%p open_upvalues=%p\n",
        vm, vm->stack, vm->open_upvalues);*/

    vm->jit->total_native_executed++;
    JitNativeFn fn = (JitNativeFn)(uintptr_t)cf->native_code;
    int rc = fn(vm);

    //printf("[AFTER-JIT] vm=%p stack=%p open_upvalues=%p rc=%d\n",
        //vm, vm->stack, vm->open_upvalues, rc);
    return rc == 0;
}

void jit_vm_print_stats(VM* vm) {
    if (!vm || !vm->jit) return;
    jit_print_stats(vm->jit);
}

