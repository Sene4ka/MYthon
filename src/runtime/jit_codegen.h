#ifndef MYTHON_JIT_CODEGEN_H
#define MYTHON_JIT_CODEGEN_H

#include "bytecode.h"
#include "vm.h"

typedef struct JitCodegen JitCodegen;

typedef int (*JitNativeFunc)(VM* vm);

JitCodegen* jit_codegen_new(int debug);
void jit_codegen_free(JitCodegen* cg);

JitNativeFunc jit_compile_function(JitCodegen* cg,
                                    Bytecode* bc,
                                    uint32_t func_idx,
                                    int debug);

size_t jit_codegen_get_size(JitCodegen* cg);

#endif