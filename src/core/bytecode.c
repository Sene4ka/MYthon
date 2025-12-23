#include "bytecode.h"
#include "utils/memory.h"
#include <string.h>
#include <stdio.h>

const uint8_t opcode_operand_length[256] = {
    [OP_NOP] = 0,
    [OP_HALT] = 0,
    [OP_ADD] = 0,
    [OP_SUB] = 0,
    [OP_MUL] = 0,
    [OP_DIV] = 0,
    [OP_MOD] = 0,
    [OP_NEG] = 0,
    [OP_NOT] = 0,
    [OP_EQ] = 0,
    [OP_NEQ] = 0,
    [OP_LT] = 0,
    [OP_LE] = 0,
    [OP_GT] = 0,
    [OP_GE] = 0,
    [OP_AND] = 0,
    [OP_OR] = 0,

    [OP_PUSH_I8] = 1,
    [OP_PUSH_NIL] = 0,
    [OP_PUSH_TRUE] = 0,
    [OP_PUSH_FALSE] = 0,
    [OP_STORE_LOCAL_8] = 1,
    [OP_LOAD_LOCAL_8] = 1,
    [OP_JUMP_8] = 2,
    [OP_JUMP_IF_TRUE_8] = 2,
    [OP_JUMP_IF_FALSE_8] = 2,
    [OP_POP] = 0,

    [OP_PUSH_I16] = 2,
    [OP_STORE_LOCAL_16] = 2,
    [OP_LOAD_LOCAL_16] = 2,
    [OP_JUMP_16] = 2,
    [OP_JUMP_IF_TRUE_16] = 2,
    [OP_JUMP_IF_FALSE_16] = 2,
    [OP_CALL_8] = 1,

    [OP_PUSH_I32] = 4,
    [OP_PUSH_F32] = 4,
    [OP_JUMP_32] = 4,
    [OP_CALL_16] = 2,
    [OP_LOAD_CONST] = 4,
    [OP_LOAD_GLOBAL] = 2,

    [OP_ARRAY_NEW] = 1,
    [OP_ARRAY_GET] = 0,
    [OP_ARRAY_SET] = 0,
    [OP_ARRAY_LEN] = 0,

    [OP_CALL_NATIVE] = 1,
    [OP_RETURN] = 0,
    [OP_RETURN_NIL] = 0,

    [OP_PRINT] = 0,
    [OP_DUP] = 0,

    [OP_BREAK] = 0,
};

Bytecode* bytecode_new(void) {
    Bytecode* bc = ALLOCATE(Bytecode, 1);

    bc->code = NULL;
    bc->capacity = 0;
    bc->length = 0;
    bc->main_locals = 0;
    bc->main_entry = 0;


    bc->constants = NULL;
    bc->const_capacity = 0;
    bc->const_count = 0;

    bc->line_numbers = NULL;
    bc->source_lines = NULL;

    bc->functions.names = NULL;
    bc->functions.addresses = NULL;
    bc->functions.local_counts = NULL;
    bc->functions.count = 0;

    return bc;
}

void bytecode_free(Bytecode* bc) {
    if (!bc) return;

    if (bc->code) {
        FREE_ARRAY(uint8_t, bc->code, bc->capacity);
    }

    for (size_t i = 0; i < bc->const_count; i++) {
        if (bc->constants[i].type == CONST_STRING && bc->constants[i].str_val) {
            free_ptr(bc->constants[i].str_val);
        }
    }
    if (bc->constants) {
        FREE_ARRAY(Constant, bc->constants, bc->const_capacity);
    }

    if (bc->line_numbers) {
        FREE_ARRAY(int, bc->line_numbers, bc->capacity);
    }

    if (bc->source_lines) {
        for (size_t i = 0; i < bc->length; i++) {
            if (bc->source_lines[i]) {
                free_ptr(bc->source_lines[i]);
            }
        }
        FREE_ARRAY(char*, bc->source_lines, bc->capacity);
    }

    for (int i = 0; i < bc->functions.count; i++) {
        if (bc->functions.names[i]) {
            free_ptr(bc->functions.names[i]);
        }
    }
    if (bc->functions.names) FREE_ARRAY(char*, bc->functions.names, bc->functions.count);
    if (bc->functions.addresses) FREE_ARRAY(size_t, bc->functions.addresses, bc->functions.count);
    if (bc->functions.local_counts) FREE_ARRAY(int, bc->functions.local_counts, bc->functions.count);

    free_ptr(bc);
}

void bc_debug_dump_header(const Bytecode* bc) {
    fprintf(stderr, "=== BYTECODE HEADER ===\n");
    fprintf(stderr, "  length      = %zu bytes\n", bc->length);
    fprintf(stderr, "  capacity    = %zu bytes\n", bc->capacity);
    fprintf(stderr, "  const_count = %zu\n", bc->const_count);
    fprintf(stderr, "  main_locals = %d\n", bc->main_locals);
    fprintf(stderr, "  main_entry  = %zu\n", (size_t)bc->main_entry);
    fprintf(stderr, "  functions   = %d\n", bc->functions.count);

    for (int i = 0; i < bc->functions.count; i++) {
        fprintf(stderr,
                "    [%d] name=\"%s\" addr=%zu locals=%d\n",
                i,
                bc->functions.names[i],
                bc->functions.addresses[i],
                bc->functions.local_counts[i]);
    }

    fprintf(stderr, "  constants:\n");
    for (size_t i = 0; i < bc->const_count; i++) {
        const Constant* c = &bc->constants[i];
        fprintf(stderr, "    #%zu: ", i);
        switch (c->type) {
            case CONST_INT:
                fprintf(stderr, "INT %lld\n", (long long)c->int_val);
                break;
            case CONST_FLOAT:
                fprintf(stderr, "FLOAT %g\n", c->float_val);
                break;
            case CONST_STRING:
                fprintf(stderr, "STRING \"%s\"\n", c->str_val);
                break;
            case CONST_FUNCTION:
                fprintf(stderr, "FUNCTION ptr=%p\n", c->ptr_val);
                break;
            case CONST_NATIVE_FN:
                fprintf(stderr, "NATIVE ptr=%p\n", c->ptr_val);
                break;
        }
    }

    fprintf(stderr, "=======================\n");
}

static void ensure_capacity(Bytecode* bc, size_t needed) {
    if (bc->capacity < bc->length + needed) {
        size_t old_capacity = bc->capacity;
        bc->capacity = GROW_CAPACITY(old_capacity);
        bc->code = GROW_ARRAY(uint8_t, bc->code, old_capacity, bc->capacity);
        bc->line_numbers = GROW_ARRAY(int, bc->line_numbers, old_capacity, bc->capacity);

        if (bc->source_lines) {
            bc->source_lines = GROW_ARRAY(char*, bc->source_lines, old_capacity, bc->capacity);
            for (size_t i = old_capacity; i < bc->capacity; i++) {
                bc->source_lines[i] = NULL;
            }
        }
    }
}

void bc_write_byte(Bytecode* bc, uint8_t byte, int line) {
    ensure_capacity(bc, 1);
    bc->code[bc->length] = byte;
    bc->line_numbers[bc->length] = line;
    bc->length++;
}

void bc_write_op(Bytecode* bc, OpCode op, int line) {
    bc_write_byte(bc, (uint8_t)op, line);
}

void bc_write_i8(Bytecode* bc, int8_t value, int line) {
    bc_write_byte(bc, (uint8_t)value, line);
}

void bc_write_i16(Bytecode* bc, int16_t value, int line) {
    bc_write_byte(bc, (value >> 8) & 0xFF, line);
    bc_write_byte(bc, value & 0xFF, line);
}

void bc_write_i32(Bytecode* bc, int32_t value, int line) {
    bc_write_byte(bc, (value >> 24) & 0xFF, line);
    bc_write_byte(bc, (value >> 16) & 0xFF, line);
    bc_write_byte(bc, (value >> 8) & 0xFF, line);
    bc_write_byte(bc, value & 0xFF, line);
}

void bc_write_u8(Bytecode* bc, uint8_t value, int line) {
    bc_write_byte(bc, value, line);
}

void bc_write_u16(Bytecode* bc, uint16_t value, int line) {
    bc_write_byte(bc, (value >> 8) & 0xFF, line);
    bc_write_byte(bc, value & 0xFF, line);
}

void bc_write_u32(Bytecode* bc, uint32_t value, int line) {
    bc_write_byte(bc, (value >> 24) & 0xFF, line);
    bc_write_byte(bc, (value >> 16) & 0xFF, line);
    bc_write_byte(bc, (value >> 8) & 0xFF, line);
    bc_write_byte(bc, value & 0xFF, line);
}

static void ensure_const_capacity(Bytecode* bc) {
    if (bc->const_capacity < bc->const_count + 1) {
        size_t old_capacity = bc->const_capacity;
        bc->const_capacity = GROW_CAPACITY(old_capacity);
        bc->constants = GROW_ARRAY(Constant, bc->constants,
                                  old_capacity, bc->const_capacity);
    }
}

int bc_add_constant_int(Bytecode* bc, int64_t value) {
    ensure_const_capacity(bc);

    for (size_t i = 0; i < bc->const_count; i++) {
        if (bc->constants[i].type == CONST_INT &&
            bc->constants[i].int_val == value) {
            return (int)i;
        }
    }

    bc->constants[bc->const_count].type = CONST_INT;
    bc->constants[bc->const_count].int_val = value;
    return (int)bc->const_count++;
}

int bc_add_constant_float(Bytecode* bc, double value) {
    ensure_const_capacity(bc);

    for (size_t i = 0; i < bc->const_count; i++) {
        if (bc->constants[i].type == CONST_FLOAT &&
            bc->constants[i].float_val == value) {
            return (int)i;
        }
    }

    bc->constants[bc->const_count].type = CONST_FLOAT;
    bc->constants[bc->const_count].float_val = value;
    return (int)bc->const_count++;
}

int bc_add_constant_string(Bytecode* bc, const char* str) {
    ensure_const_capacity(bc);

    for (size_t i = 0; i < bc->const_count; i++) {
        if (bc->constants[i].type == CONST_STRING &&
            strcmp(bc->constants[i].str_val, str) == 0) {
            return (int)i;
        }
    }

    bc->constants[bc->const_count].type = CONST_STRING;
    bc->constants[bc->const_count].str_val = allocate(strlen(str) + 1);
    strcpy(bc->constants[bc->const_count].str_val, str);
    return (int)bc->const_count++;
}

int bc_add_constant_ptr(Bytecode* bc, void* ptr, int type) {
    ensure_const_capacity(bc);

    bc->constants[bc->const_count].type = type;
    bc->constants[bc->const_count].ptr_val = ptr;
    return (int)bc->const_count++;
}

void bc_add_function(Bytecode* bc, const char* name, size_t address, int locals) {
    int old_count = bc->functions.count;
    bc->functions.count++;

    bc->functions.names = GROW_ARRAY(char*, bc->functions.names,
                                    old_count, bc->functions.count);
    bc->functions.addresses = GROW_ARRAY(size_t, bc->functions.addresses,
                                       old_count, bc->functions.count);
    bc->functions.local_counts = GROW_ARRAY(int, bc->functions.local_counts,
                                          old_count, bc->functions.count);

    bc->functions.names[old_count] = allocate(strlen(name) + 1);
    strcpy(bc->functions.names[old_count], name);
    bc->functions.addresses[old_count] = address;
    //if (locals < 8) locals = 8;
    bc->functions.local_counts[old_count] = locals;
}

size_t bc_get_current_address(Bytecode* bc) {
    return bc->length;
}

void bc_patch_i16(Bytecode* bc, size_t address, int16_t value) {
    if (address + 1 >= bc->length) return;

    bc->code[address] = (value >> 8) & 0xFF;
    bc->code[address + 1] = value & 0xFF;
}

void bc_patch_i32(Bytecode* bc, size_t address, int32_t value) {
    if (address + 3 >= bc->length) return;

    bc->code[address] = (value >> 24) & 0xFF;
    bc->code[address + 1] = (value >> 16) & 0xFF;
    bc->code[address + 2] = (value >> 8) & 0xFF;
    bc->code[address + 3] = value & 0xFF;
}

void bc_disassemble(const Bytecode* bc) {
    printf("=== Байткод (%zu байт) ===\n", bc->length);
    printf("Константы: %zu\n", bc->const_count);
    printf("Функции: %d\n", bc->functions.count);

    size_t ip = 0;
    while (ip < bc->length) {
        printf("%04zu: ", ip);

        if (ip < bc->length) {
            printf("L%04d ", bc->line_numbers[ip]);
        }

        uint8_t opcode = bc->code[ip];
        printf("%02X ", opcode);

        uint8_t operand_len = opcode_operand_length[opcode];

        for (int i = 0; i < operand_len; i++) {
            if (ip + 1 + i < bc->length) {
                printf("%02X ", bc->code[ip + 1 + i]);
            }
        }

        for (int i = operand_len; i < 4; i++) {
            printf("   ");
        }

        printf(" ; ");

        switch (opcode) {
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

            case OP_PUSH_TRUE:  printf("PUSH_TRUE"); break;
            case OP_PUSH_FALSE: printf("PUSH_FALSE"); break;
            case OP_LOAD_GLOBAL: {
                uint16_t idx = (bc->code[ip + 1] << 8) | bc->code[ip + 2];
                printf("LOAD_GLOBAL %u", idx);
                break;
            }
            case OP_ARRAY_GET:  printf("ARRAY_GET"); break;
            case OP_ARRAY_SET:  printf("ARRAY_SET"); break;
            case OP_ARRAY_LEN:  printf("ARRAY_LEN"); break;

            case OP_PUSH_I8: {
                int8_t val = (int8_t)bc->code[ip + 1];
                printf("PUSH_I8 %d", val);
                break;
            }

            case OP_PUSH_I16: {
                int16_t val = (bc->code[ip + 1] << 8) | bc->code[ip + 2];
                printf("PUSH_I16 %d", val);
                break;
            }

            case OP_PUSH_I32: {
                int32_t val = (bc->code[ip + 1] << 24) |
                             (bc->code[ip + 2] << 16) |
                             (bc->code[ip + 3] << 8) |
                             bc->code[ip + 4];
                printf("PUSH_I32 %d", val);
                break;
            }

            case OP_LOAD_CONST: {
                uint32_t idx = (bc->code[ip + 1] << 24) |
                              (bc->code[ip + 2] << 16) |
                              (bc->code[ip + 3] << 8) |
                              bc->code[ip + 4];
                printf("LOAD_CONST #%u", idx);
                if (idx < bc->const_count) {
                    printf(" (");
                    const Constant* c = &bc->constants[idx];
                    switch (c->type) {
                        case CONST_INT: printf("%lld", c->int_val); break;
                        case CONST_FLOAT: printf("%g", c->float_val); break;
                        case CONST_STRING: printf("\"%s\"", c->str_val); break;
                        default: printf("ptr");
                    }
                    printf(")");
                }
                break;
            }

            case OP_STORE_LOCAL_8: {
                uint8_t idx = bc->code[ip + 1];
                printf("STORE_LOCAL_8 %u", idx);
                break;
            }

            case OP_LOAD_LOCAL_8: {
                uint8_t idx = bc->code[ip + 1];
                printf("LOAD_LOCAL_8 %u", idx);
                break;
            }

            case OP_JUMP_8: {
                int16_t offset = (int16_t)((bc->code[ip + 1] << 8) | bc->code[ip + 2]);
                printf("JUMP_8 %+d -> %zu", offset, ip + 3 + offset);
                break;
            }

            case OP_JUMP_16: {
                int16_t offset = (bc->code[ip + 1] << 8) | bc->code[ip + 2];
                printf("JUMP_16 %+d -> %zu", offset, ip + 3 + offset);
                break;
            }

            case OP_JUMP_IF_FALSE_8: {
                int16_t offset = (int16_t)((bc->code[ip + 1] << 8) | bc->code[ip + 2]);
                printf("JUMP_IF_FALSE_8 %+d -> %zu", offset, ip + 3 + offset);
                break;
            }

            case OP_CALL_8: {
                uint8_t args = bc->code[ip + 1];
                printf("CALL_8 args=%u", args);
                break;
            }

            case OP_RETURN: printf("RETURN"); break;
            case OP_RETURN_NIL: printf("RETURN_NIL"); break;
            case OP_PRINT: printf("PRINT"); break;

            case OP_ARRAY_NEW: {
                uint8_t size = bc->code[ip + 1];
                printf("ARRAY_NEW size=%u", size);
                break;
            }

            default: printf("UNKNOWN_%02X", opcode);
        }

        printf("\n");
        ip += 1 + operand_len;
    }

    printf("=== Конец байткода ===\n");
}

int bc_save_to_file(const Bytecode* bc, const char* filename) {
    FILE* f = fopen(filename, "wb");
    if (!f) return 0;

    uint32_t magic = 0x4D595448;
    uint16_t version = 0x0001;
    fwrite(&magic, sizeof(uint32_t), 1, f);
    fwrite(&version, sizeof(uint16_t), 1, f);

    int32_t main_locals = bc->main_locals;
    fwrite(&main_locals, sizeof(int32_t), 1, f);

    uint32_t code_size = (uint32_t)bc->length;
    fwrite(&code_size, sizeof(uint32_t), 1, f);

    fwrite(bc->code, sizeof(uint8_t), bc->length, f);

    fwrite(bc->line_numbers, sizeof(int), bc->length, f);

    uint32_t const_count = (uint32_t)bc->const_count;
    fwrite(&const_count, sizeof(uint32_t), 1, f);

    for (size_t i = 0; i < bc->const_count; i++) {
        const Constant* c = &bc->constants[i];
        fwrite(&c->type, sizeof(uint8_t), 1, f);

        switch (c->type) {
            case CONST_INT:
                fwrite(&c->int_val, sizeof(int64_t), 1, f);
                break;
            case CONST_FLOAT:
                fwrite(&c->float_val, sizeof(double), 1, f);
                break;
            case CONST_STRING: {
                uint32_t len = (uint32_t)strlen(c->str_val);
                fwrite(&len, sizeof(uint32_t), 1, f);
                fwrite(c->str_val, sizeof(char), len, f);
                break;
            }
            case CONST_FUNCTION:
            case CONST_NATIVE_FN:
                break;
        }
    }

    uint32_t func_count = (uint32_t)bc->functions.count;
    fwrite(&func_count, sizeof(uint32_t), 1, f);

    for (int i = 0; i < bc->functions.count; i++) {
        uint32_t name_len = (uint32_t)strlen(bc->functions.names[i]);
        fwrite(&name_len, sizeof(uint32_t), 1, f);
        fwrite(bc->functions.names[i], sizeof(char), name_len, f);

        fwrite(&bc->functions.addresses[i], sizeof(size_t), 1, f);
        fwrite(&bc->functions.local_counts[i], sizeof(int), 1, f);
    }

    fclose(f);
    return 1;
}

Bytecode* bc_load_from_file(const char* filename) {
    FILE* f = fopen(filename, "rb");
    if (!f) return NULL;

    uint32_t magic;
    fread(&magic, sizeof(uint32_t), 1, f);
    if (magic != 0x4D595448) {
        fclose(f);
        return NULL;
    }

    uint16_t version;
    fread(&version, sizeof(uint16_t), 1, f);

    Bytecode* bc = bytecode_new();

    int32_t main_locals;
    fread(&main_locals, sizeof(int32_t), 1, f);
    bc->main_locals = main_locals;

    uint32_t code_size;
    fread(&code_size, sizeof(uint32_t), 1, f);

    bc->capacity = code_size;
    bc->length = code_size;
    bc->code = ALLOCATE(uint8_t, bc->capacity);
    bc->line_numbers = ALLOCATE(int, bc->capacity);

    fread(bc->code, sizeof(uint8_t), code_size, f);

    fread(bc->line_numbers, sizeof(int), code_size, f);

    uint32_t const_count;
    fread(&const_count, sizeof(uint32_t), 1, f);

    for (uint32_t i = 0; i < const_count; i++) {
        uint8_t type;
        fread(&type, sizeof(uint8_t), 1, f);

        Constant c;
        c.type = type;

        switch (type) {
            case CONST_INT:
                fread(&c.int_val, sizeof(int64_t), 1, f);
                break;
            case CONST_FLOAT:
                fread(&c.float_val, sizeof(double), 1, f);
                break;
            case CONST_STRING: {
                uint32_t len;
                fread(&len, sizeof(uint32_t), 1, f);
                c.str_val = allocate(len + 1);
                fread(c.str_val, sizeof(char), len, f);
                c.str_val[len] = '\0';
                break;
            }
            case CONST_FUNCTION:
            case CONST_NATIVE_FN:
                c.ptr_val = NULL;
                break;
        }

        ensure_const_capacity(bc);
        bc->constants[bc->const_count++] = c;
    }

    uint32_t func_count;
    fread(&func_count, sizeof(uint32_t), 1, f);

    for (uint32_t i = 0; i < func_count; i++) {
        uint32_t name_len;
        fread(&name_len, sizeof(uint32_t), 1, f);

        char* name = allocate(name_len + 1);
        fread(name, sizeof(char), name_len, f);
        name[name_len] = '\0';

        size_t address;
        int locals;
        fread(&address, sizeof(size_t), 1, f);
        fread(&locals, sizeof(int), 1, f);

        bc_add_function(bc, name, address, locals);
        free_ptr(name);
    }

    fclose(f);
    return bc;
}
