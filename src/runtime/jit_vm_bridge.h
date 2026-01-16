#ifndef MYTHON_JIT_VM_BRIDGE_H
#define MYTHON_JIT_VM_BRIDGE_H

#include "vm.h"
#include "jit.h"

bool jit_vm_handle_call(VM* vm, uint32_t func_idx, Bytecode* bytecode);
void jit_vm_init(VM* vm, int debug, uint64_t opt_threshold, uint64_t native_threshold, int opt_level);
void jit_vm_cleanup(VM* vm);
void jit_vm_print_stats(VM* vm);

#endif
