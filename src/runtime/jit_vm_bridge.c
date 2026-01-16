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

    jit_increment_hotness(vm->jit, func_idx);

    CompiledFunction* cf = jit_get_compiled_func(vm->jit, func_idx);

    if (cf && cf->tier == TIER_NATIVE && cf->native_code) {
        vm->jit->total_native_executed++;
        JitNativeFn fn = (JitNativeFn)(uintptr_t)cf->native_code;
        int rc = fn(vm);
        return rc == 0;
    }

    if (!jit_should_compile(vm->jit, func_idx)) {
        vm->jit->total_interp_executed++;
        return false;
    }

    void* result = jit_compile_or_promote(vm->jit, bytecode, func_idx);

    if (!result) {
        vm->jit->total_interp_executed++;
        return false;
    }

    cf = jit_get_compiled_func(vm->jit, func_idx);

    if (!cf || cf->tier != TIER_NATIVE || !cf->native_code) {
        vm->jit->total_interp_executed++;
        return false;
    }

    if (vm->jit->debug) {
        printf("[JIT-FIRST-EXEC] func=%u native=%p\n", func_idx, cf->native_code);
    }

    vm->jit->total_native_executed++;
    JitNativeFn fn = (JitNativeFn)(uintptr_t)cf->native_code;
    int rc = fn(vm);

    return rc == 0;
}

void jit_vm_print_stats(VM* vm) {
    if (!vm || !vm->jit) return;
    jit_print_stats(vm->jit);
}

