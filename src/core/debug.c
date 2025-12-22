#include "debug.h"
#include "utils/memory.h"
#include <stdio.h>
#include <stdint.h>

void disassemble_instruction(const Bytecode* bc, size_t offset) {
    if (offset >= bc->length) {
        printf("Ошибка: offset за пределами кода\n");
        return;
    }

    uint8_t* ip = bc->code + offset;
    uint8_t op = *ip;

    printf("%04zu ", offset);

    int line = 0;
    if (offset < bc->length && bc->line_numbers) {
        line = bc->line_numbers[offset];
        printf("%4d ", line);
    } else {
        printf("   - ");
    }

    switch (op) {
        case OP_NOP: printf("NOP"); break;
        case OP_HALT: printf("HALT"); break;
        case OP_ADD: printf("ADD"); break;
        case OP_SUB: printf("SUB"); break;
        case OP_MUL: printf("MUL"); break;
        case OP_DIV: printf("DIV"); break;
        case OP_MOD: printf("MOD"); break;
        case OP_NEG: printf("NEG"); break;
        case OP_NOT: printf("NOT"); break;
        case OP_EQ: printf("EQ"); break;
        case OP_NEQ: printf("NEQ"); break;
        case OP_LT: printf("LT"); break;
        case OP_LE: printf("LE"); break;
        case OP_GT: printf("GT"); break;
        case OP_GE: printf("GE"); break;
        case OP_AND: printf("AND"); break;
        case OP_OR: printf("OR"); break;

        case OP_PUSH_I8: printf("PUSH_I8 %d", (int8_t)ip[1]); break;
        case OP_PUSH_NIL: printf("PUSH_NIL"); break;
        case OP_PUSH_TRUE: printf("PUSH_TRUE"); break;
        case OP_PUSH_FALSE: printf("PUSH_FALSE"); break;
        case OP_STORE_LOCAL_8: printf("STORE_LOCAL_8 %d", (uint8_t)ip[1]); break;
        case OP_LOAD_LOCAL_8: printf("LOAD_LOCAL_8 %d", (uint8_t)ip[1]); break;
        case OP_JUMP_8: printf("JUMP_8 %d", (int8_t)ip[1]); break;
        case OP_JUMP_IF_TRUE_8: printf("JUMP_IF_TRUE_8 %d", (int8_t)ip[1]); break;
        case OP_JUMP_IF_FALSE_8: printf("JUMP_IF_FALSE_8 %d", (int8_t)ip[1]); break;
        case OP_POP: printf("POP"); break;

        case OP_PUSH_I16: {
            int16_t val = (ip[1] << 8) | ip[2];
            printf("PUSH_I16 %d", val);
            break;
        }
        case OP_STORE_LOCAL_16: {
            int16_t val = (ip[1] << 8) | ip[2];
            printf("STORE_LOCAL_16 %d", val);
            break;
        }
        case OP_LOAD_LOCAL_16: {
            int16_t val = (ip[1] << 8) | ip[2];
            printf("LOAD_LOCAL_16 %d", val);
            break;
        }
        case OP_JUMP_16: {
            int16_t val = (ip[1] << 8) | ip[2];
            printf("JUMP_16 %d", val);
            break;
        }
        case OP_JUMP_IF_TRUE_16: {
            int16_t val = (ip[1] << 8) | ip[2];
            printf("JUMP_IF_TRUE_16 %d", val);
            break;
        }
        case OP_JUMP_IF_FALSE_16: {
            int16_t val = (ip[1] << 8) | ip[2];
            printf("JUMP_IF_FALSE_16 %d", val);
            break;
        }
        case OP_CALL_8: printf("CALL_8 %d", (uint8_t)ip[1]); break;

        case OP_PUSH_I32: {
            int32_t val = (ip[1] << 24) | (ip[2] << 16) | (ip[3] << 8) | ip[4];
            printf("PUSH_I32 %d", val);
            break;
        }
        case OP_PUSH_F32: {
            union {
                uint32_t i;
                float f;
            } u;
            u.i = (ip[1] << 24) | (ip[2] << 16) | (ip[3] << 8) | ip[4];
            printf("PUSH_F32 %g", u.f);
            break;
        }
        case OP_JUMP_32: {
            int32_t val = (ip[1] << 24) | (ip[2] << 16) | (ip[3] << 8) | ip[4];
            printf("JUMP_32 %d", val);
            break;
        }
        case OP_CALL_16: {
            int16_t val = (ip[1] << 8) | ip[2];
            printf("CALL_16 %d", val);
            break;
        }
        case OP_LOAD_CONST: {
            int32_t idx = (ip[1] << 24) | (ip[2] << 16) | (ip[3] << 8) | ip[4];
            printf("LOAD_CONST %d", idx);
            break;
        }

        case OP_ARRAY_NEW: printf("ARRAY_NEW"); break;
        case OP_ARRAY_GET: printf("ARRAY_GET"); break;
        case OP_ARRAY_SET: printf("ARRAY_SET"); break;
        case OP_ARRAY_LEN: printf("ARRAY_LEN"); break;

        case OP_CALL_NATIVE: printf("CALL_NATIVE"); break;
        case OP_RETURN: printf("RETURN"); break;
        case OP_RETURN_NIL: printf("RETURN_NIL"); break;

        case OP_PRINT: printf("PRINT"); break;
        case OP_DUP: printf("DUP"); break;

        case OP_BREAK: printf("BREAK"); break;

        default: printf("UNKNOWN_%02X", op); break;
    }

    printf("\n");
}

void disassemble_bytecode(const Bytecode* bc) {
    printf("=== Байт-код (%zu байт) ===\n", bc->length);

    size_t offset = 0;
    while (offset < bc->length) {
        disassemble_instruction(bc, offset);
        offset += 1 + opcode_operand_length[bc->code[offset]];
    }

    printf("=== Константы (%zu) ===\n", bc->const_count);
    for (size_t i = 0; i < bc->const_count; i++) {
        const Constant* c = &bc->constants[i];
        printf("%4zu: ", i);
        switch (c->type) {
            case CONST_INT: printf("INT %lld", (long long)c->int_val); break;
            case CONST_FLOAT: printf("FLOAT %g", c->float_val); break;
            case CONST_STRING: printf("STRING \"%s\"", c->str_val); break;
            case CONST_FUNCTION: printf("FUNCTION %p", c->ptr_val); break;
            case CONST_NATIVE_FN: printf("NATIVE_FN %p", c->ptr_val); break;
            default: printf("UNKNOWN"); break;
        }
        printf("\n");
    }
}

void dump_stack(const VM* vm) {
    printf("=== Стек (sp=%d, size=%d) ===\n", vm->sp, vm->stack_size);

    for (int i = 0; i < vm->sp; i++) {
        printf("[%d] ", i);
        vm_print_value(vm->stack[i]);
        printf("\n");
    }

    if (vm->sp == 0) {
        printf("(пусто)\n");
    }
}

void dump_frames(const VM* vm) {
    printf("=== Фреймы (%d) ===\n", vm->frame_count);

    for (int i = 0; i < vm->frame_count; i++) {
        const CallFrame* frame = &vm->frames[i];
        printf("Фрейм %d: ip=%p, slots=%d\n",
               i, (void*)(frame->ip - frame->bytecode->code), frame->slot_count);
    }
}

void dump_globals(const VM* vm) {
    printf("=== Глобальные (%d) ===\n", vm->global_count);

    for (int i = 0; i < vm->global_count; i++) {
        if (!IS_NIL(vm->globals[i])) {
            printf("[%d] ", i);
            vm_print_value(vm->globals[i]);
            printf("\n");
        }
    }
}

void trace_execution(const VM* vm, const uint8_t* ip) {
    if (vm->frame_count == 0) return;

    const CallFrame* frame = &vm->frames[vm->frame_count - 1];
    size_t offset = ip - frame->bytecode->code;

    printf("Выполнение: offset=%zu, sp=%d | ", offset, vm->sp);

    if (vm->sp > 0) {
        vm_print_value(vm_peek((VM*)vm, 0));
    } else {
        printf("(пусто)");
    }

    printf("\n");
}