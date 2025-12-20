#ifndef MYTHON_DEBUG_H
#define MYTHON_DEBUG_H

#include "vm.h"
#include "bytecode.h"

void disassemble_instruction(const Bytecode* bc, size_t offset);
void disassemble_bytecode(const Bytecode* bc);
void dump_stack(const VM* vm);
void dump_frames(const VM* vm);
void dump_globals(const VM* vm);
void trace_execution(const VM* vm, const uint8_t* ip);

#endif