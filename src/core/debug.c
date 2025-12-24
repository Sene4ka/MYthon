#include "debug.h"
#include <stdio.h>

static int disassemble_instruction_internal(const Bytecode* bc, size_t offset) {
    if (offset >= bc->code_size) {
        printf("Ошибка: offset за пределами кода\n");
        return (int)offset;
    }

    const uint8_t* ip = bc->code + offset;
    OpCode op = (OpCode)*ip;

    printf("%04zu ", offset);

    int line = -1;
    if (bc->line_numbers && offset < bc->code_size) {
        line = bc->line_numbers[offset];
    }
    if (line >= 0) printf("%4d ", line);
    else           printf("   - ");

    switch (op) {
        /* stack/arith/logical */
        case OP_NOP:  printf("NOP"); return (int)(offset + 1);
        case OP_HALT: printf("HALT"); return (int)(offset + 1);

        case OP_ADD:  printf("ADD");  return (int)(offset + 1);
        case OP_SUB:  printf("SUB");  return (int)(offset + 1);
        case OP_MUL:  printf("MUL");  return (int)(offset + 1);
        case OP_DIV:  printf("DIV");  return (int)(offset + 1);
        case OP_MOD:  printf("MOD");  return (int)(offset + 1);

        case OP_NEG:  printf("NEG");  return (int)(offset + 1);
        case OP_NOT:  printf("NOT");  return (int)(offset + 1);
        case OP_DUP:  printf("DUP");  return (int)(offset + 1);
        case OP_POP:  printf("POP");  return (int)(offset + 1);

        case OP_EQ:   printf("EQ");   return (int)(offset + 1);
        case OP_NEQ:  printf("NEQ");  return (int)(offset + 1);
        case OP_LT:   printf("LT");   return (int)(offset + 1);
        case OP_LE:   printf("LE");   return (int)(offset + 1);
        case OP_GT:   printf("GT");   return (int)(offset + 1);
        case OP_GE:   printf("GE");   return (int)(offset + 1);
        case OP_AND:  printf("AND");  return (int)(offset + 1);
        case OP_OR:   printf("OR");   return (int)(offset + 1);

        /* constants */
        case OP_LOAD_CONST_U16: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            printf("LOAD_CONST_U16 %u", idx);
            return (int)(offset + 3);
        }

        /* locals/upvalues */
        case OP_LOAD_LOCAL_U8: {
            uint8_t idx = ip[1];
            printf("LOAD_LOCAL_U8 %u", idx);
            return (int)(offset + 2);
        }
        case OP_STORE_LOCAL_U8: {
            uint8_t idx = ip[1];
            printf("STORE_LOCAL_U8 %u", idx);
            return (int)(offset + 2);
        }
        case OP_LOAD_UPVALUE_U8: {
            uint8_t idx = ip[1];
            printf("LOAD_UPVALUE_U8 %u", idx);
            return (int)(offset + 2);
        }
        case OP_STORE_UPVALUE_U8: {
            uint8_t idx = ip[1];
            printf("STORE_UPVALUE_U8 %u", idx);
            return (int)(offset + 2);
        }

        /* jumps */
        case OP_JUMP_U16:
        case OP_JUMP_IF_FALSE_U16:
        case OP_JUMP_IF_TRUE_U16: {
            uint16_t off = (uint16_t)(ip[1] << 8 | ip[2]);
            const char* name =
                (op == OP_JUMP_U16) ? "JUMP_U16" :
                (op == OP_JUMP_IF_FALSE_U16) ? "JUMP_IF_FALSE_U16" :
                                               "JUMP_IF_TRUE_U16";
            printf("%s %d", name, (int16_t)off);
            return (int)(offset + 3);
        }

        /* calls */
        case OP_CALL_U8: {
            uint8_t argc = ip[1];
            printf("CALL_U8 %u", argc);
            return (int)(offset + 2);
        }
        case OP_CALL_U16: {
            uint16_t argc = (uint16_t)(ip[1] << 8 | ip[2]);
            printf("CALL_U16 %u", argc);
            return (int)(offset + 3);
        }

        case OP_RETURN:     printf("RETURN");     return (int)(offset + 1);
        case OP_RETURN_NIL: printf("RETURN_NIL"); return (int)(offset + 1);

        /* closures/classes */
        case OP_NEW_CLOSURE: printf("NEW_CLOSURE"); return (int)(offset + 1);
        case OP_NEW_CLASS:   printf("NEW_CLASS");   return (int)(offset + 1);

        case OP_LOAD_FIELD_U8: {
            uint8_t idx = ip[1];
            printf("LOAD_FIELD_U8 %u", idx);
            return (int)(offset + 2);
        }
        case OP_STORE_FIELD_U8: {
            uint8_t idx = ip[1];
            printf("STORE_FIELD_U8 %u", idx);
            return (int)(offset + 2);
        }
        case OP_CALL_METHOD_U8: {
            uint8_t argc = ip[1];
            printf("CALL_METHOD_U8 %u", argc);
            return (int)(offset + 2);
        }

        /* arrays */
        case OP_ARRAY_NEW_U8: {
            uint8_t count = ip[1];
            printf("ARRAY_NEW_U8 %u", count);
            return (int)(offset + 2);
        }
        case OP_ARRAY_GET: printf("ARRAY_GET"); return (int)(offset + 1);
        case OP_ARRAY_SET: printf("ARRAY_SET"); return (int)(offset + 1);
        case OP_ARRAY_LEN: printf("ARRAY_LEN"); return (int)(offset + 1);

        /* globals */
        case OP_LOAD_GLOBAL_U16: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            printf("LOAD_GLOBAL_U16 %u", idx);
            return (int)(offset + 3);
        }
        case OP_STORE_GLOBAL_U16: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            printf("STORE_GLOBAL_U16 %u", idx);
            return (int)(offset + 3);
        }

        /* misc */
        case OP_PRINT: printf("PRINT"); return (int)(offset + 1);
        case OP_BREAK: printf("BREAK"); return (int)(offset + 1);

        default:
            printf("UNKNOWN_0x%02X", (unsigned)op);
            return (int)(offset + 1);
    }
}

void disassemble_instruction(const Bytecode* bc, size_t offset) {
    disassemble_instruction_internal(bc, offset);
    printf("\n");
}

void disassemble_bytecode(const Bytecode* bc) {
    printf("=== Байт-код (%zu байт) ===\n", bc->code_size);

    size_t offset = 0;
    while (offset < bc->code_size) {
        offset = (size_t)disassemble_instruction_internal(bc, offset);
        printf("\n");
    }

    printf("=== Константы (%zu) ===\n", bc->const_count);
    for (size_t i = 0; i < bc->const_count; i++) {
        const Constant* c = &bc->constants[i];
        printf("%4zu: ", i);
        switch (c->type) {
            case CONST_INT:
                printf("INT %lld", (long long)c->int_val);
                break;
            case CONST_FLOAT:
                printf("FLOAT %g", c->float_val);
                break;
            case CONST_STRING:
                printf("STRING \"%s\"", c->str_val);
                break;
            case CONST_CLOSURE:
                printf("CLOSURE func=%u upvalues=%u",
                       c->closure.func_idx, c->closure.upvalue_count);
                break;
            case CONST_CLASS:
                printf("CLASS idx=%u", c->class_ref.class_idx);
                break;
            case CONST_NATIVE_FN:
                printf("NATIVE_FN %p", c->native_ptr);
                break;
        }
        printf("\n");
    }
}
