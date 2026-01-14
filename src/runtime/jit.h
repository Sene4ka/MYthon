#ifndef MYTHON_JIT_H
#define MYTHON_JIT_H

#include "bytecode.h"
#include <stdint.h>

typedef struct VM VM;

extern VM* jit_vm;

typedef enum {
    TIER_INTERPRETER = 0,
    TIER_OPTIMIZED_BC,
    TIER_NATIVE
} CompilationTier;

typedef int (*JitNativeFunc)(VM* vm);

typedef struct {
    uint32_t        func_idx;
    CompilationTier tier;
    union {
        struct {
            uint32_t original_code_start;
            uint32_t original_code_end;
        } optimized_bc;
        JitNativeFunc  native_code;
    };
    size_t   code_size;
    uint64_t promotion_count;
    int      native_failed;
} CompiledFunction;

typedef enum {
    JIT_OPT_NONE          = 0,
    JIT_OPT_CONSTANT_FOLD = 1 << 0,
    JIT_OPT_PEEPHOLE      = 1 << 1,
    JIT_OPT_DEAD_CODE     = 1 << 2,
    JIT_OPT_ALL           = JIT_OPT_CONSTANT_FOLD |
                             JIT_OPT_PEEPHOLE |
                             JIT_OPT_DEAD_CODE
} JITOptFlags;

typedef struct JIT {
    int        enabled;
    int        debug;

    uint64_t   optimize_threshold;
    uint64_t   native_threshold;

    JITOptFlags      opt_flags;

    CompiledFunction* compiled_funcs;
    size_t            compiled_count;
    size_t            compiled_capacity;

    uint64_t* hotness_counters;
    size_t    counter_count;

    uint64_t total_calls;
    uint64_t total_optimized;
    uint64_t total_native;
    uint64_t total_native_executed;
    uint64_t total_interp_executed;
} JIT;

JIT*            jit_new(void);
void            jit_free(JIT* jit);

void            jit_increment_hotness(JIT* jit, uint32_t func_idx);
CompilationTier jit_get_target_tier(JIT* jit, uint32_t func_idx);
int             jit_should_compile(JIT* jit, uint32_t func_idx);

void*           jit_compile_or_promote(JIT* jit, Bytecode* bc, uint32_t func_idx);

CompiledFunction* jit_get_compiled_func(JIT* jit, uint32_t func_idx);
void              jit_print_stats(JIT* jit);

int jit_optimize_bytecode(JIT* jit, Bytecode* bc, uint32_t func_idx);
int jit_bc_constant_folding(Bytecode* bc, uint32_t func_idx, int debug);
//int jit_bc_peephole(Bytecode* bc, uint32_t func_idx, int debug);
//int jit_bc_dead_code_elimination(Bytecode* bc, uint32_t func_idx, int debug);

#endif
