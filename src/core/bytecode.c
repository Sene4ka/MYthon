#include "bytecode.h"

#include <stdio.h>

#include "utils/memory.h"
#include <string.h>
#include <stdlib.h>

const uint8_t opcode_operand_length[256] = {
    [OP_NOP]  = 0,
    [OP_HALT] = 0,

    // Stack / logic
    [OP_ADD] = 0, [OP_SUB] = 0, [OP_MUL] = 0, [OP_DIV] = 0, [OP_MOD] = 0,
    [OP_NEG] = 0, [OP_NOT] = 0, [OP_DUP] = 0, [OP_POP] = 0,
    [OP_EQ] = 0, [OP_NEQ] = 0, [OP_LT] = 0, [OP_LE] = 0, [OP_GT] = 0, [OP_GE] = 0,
    [OP_AND] = 0, [OP_OR]  = 0,

    // Const
    [OP_LOAD_CONST_U16] = 2,   // index

    // Locals / upvalues
    [OP_LOAD_LOCAL_U8]   = 1,  // local index
    [OP_STORE_LOCAL_U8]  = 1,
    [OP_LOAD_UPVALUE_U8] = 1,  // upvalue index
    [OP_STORE_UPVALUE_U8]= 1,

    // Jumps
    [OP_JUMP_U16]         = 2,
    [OP_JUMP_IF_FALSE_U16]= 2,
    [OP_JUMP_IF_TRUE_U16] = 2,

    // Calls
    [OP_CALL_U8]  = 1,         // argc
    [OP_CALL_U16] = 2,         // argc (расширенный)
    [OP_RETURN]   = 0,
    [OP_RETURN_NIL] = 0,

    // Closures / classes
    [OP_NEW_CLOSURE]   = 0,    // func_idx (U16 в таблице функций)
    [OP_NEW_CLASS]     = 2,    // class_idx (U16 в таблице классов)
    [OP_LOAD_FIELD_U8] = 1,    // field_idx
    [OP_STORE_FIELD_U8]= 1,
    [OP_CALL_METHOD_U8]= 1,    // method_idx, argc берём из стека/след. опкода по дизайну

    // Arrays
    [OP_ARRAY_NEW_U8] = 1,     // initial size
    [OP_ARRAY_GET]    = 0,
    [OP_ARRAY_SET]    = 0,
    [OP_ARRAY_LEN]    = 0,

    // Globals
    [OP_LOAD_GLOBAL_U16]  = 2, // index
    [OP_STORE_GLOBAL_U16] = 2,

    // Debug
    [OP_PRINT] = 0,
    [OP_BREAK] = 0,
};

static void ensure_code_capacity(Bytecode* bc, size_t needed) {
    if (bc->code_capacity >= bc->code_size + needed) return;

    size_t old_capacity = bc->code_capacity;
    bc->code_capacity = GROW_CAPACITY(old_capacity);
    bc->code = GROW_ARRAY(uint8_t, bc->code, old_capacity, bc->code_capacity);

    bc->line_numbers = GROW_ARRAY(int, bc->line_numbers, old_capacity, bc->code_capacity);
}

Bytecode* bytecode_new(void) {
    Bytecode* bc = ALLOCATE(Bytecode, 1);
    bc->code = NULL;
    bc->code_capacity = 0;
    bc->code_size = 0;

    bc->constants = NULL;
    bc->const_capacity = 0;
    bc->const_count = 0;

    bc->functions = NULL;
    bc->func_capacity = 0;
    bc->func_count = 0;

    bc->globals = NULL;
    bc->global_const_indices = NULL;
    bc->global_count = 0;
    bc->global_capacity = 0;

    bc->classes = NULL;
    bc->class_capacity = 0;
    bc->class_count = 0;

    bc->main_closure_idx = 0;
    bc->gc_roots = NULL;
    bc->gc_roots_count = 0;
    bc->line_numbers = NULL;

    return bc;
}

void bytecode_free(Bytecode* bc) {
    if (!bc) return;

    FREE_ARRAY(uint8_t, bc->code, bc->code_capacity);
    FREE_ARRAY(int, bc->line_numbers, bc->code_capacity);

    for (size_t i = 0; i < bc->const_count; i++) {
        if (bc->constants[i].type == CONST_STRING && bc->constants[i].str_val) {
            FREE_ARRAY(char, bc->constants[i].str_val,
                       strlen(bc->constants[i].str_val) + 1);
        }
    }
    FREE_ARRAY(Constant, bc->constants, bc->const_capacity);

    for (size_t i = 0; i < bc->func_count; i++) {
        free(bc->functions[i].name);
        FREE_ARRAY(UpvalueInfo, bc->functions[i].upvalues, bc->functions[i].n_upvalues);
    }
    FREE_ARRAY(Function, bc->functions, bc->func_capacity);

    /* globals */
    if (bc->globals) {
        for (size_t i = 0; i < bc->global_count; i++) {
            if (bc->globals[i]) {
                FREE_ARRAY(char, bc->globals[i], strlen(bc->globals[i]) + 1);
            }
        }
    }
    FREE_ARRAY(char*, bc->globals, bc->global_capacity);
    FREE_ARRAY(uint16_t, bc->global_const_indices, bc->global_capacity);

    for (size_t i = 0; i < bc->class_count; i++) {
        for (size_t j = 0; j < bc->classes[i].n_fields; j++) {
            free(bc->classes[i].field_names[j]);
        }
        for (size_t j = 0; j < bc->classes[i].n_methods; j++) {
            free(bc->classes[i].method_names[j]);
        }
        FREE_ARRAY(char*, bc->classes[i].field_names, bc->classes[i].n_fields);
        FREE_ARRAY(char*, bc->classes[i].method_names, bc->classes[i].n_methods);
        FREE_ARRAY(uint16_t, bc->classes[i].field_indices, bc->classes[i].n_fields);
    }
    FREE_ARRAY(ClassTemplate, bc->classes, bc->class_capacity);

    FREE_ARRAY(uint32_t, bc->gc_roots, bc->gc_roots_count);
    free(bc);
}


void bc_write_op(Bytecode* bc, OpCode op, int line) {
    ensure_code_capacity(bc, 1 + opcode_operand_length[op]);
    bc->code[bc->code_size] = (uint8_t)op;
    bc->line_numbers[bc->code_size] = line;
    bc->code_size++;
}

void bc_write_u8(Bytecode* bc, uint8_t val, int line) {
    ensure_code_capacity(bc, 1);
    bc->code[bc->code_size] = val;
    bc->line_numbers[bc->code_size] = line;
    bc->code_size++;
}

void bc_write_u16(Bytecode* bc, uint16_t val, int line) {
    ensure_code_capacity(bc, 2);
    bc->code[bc->code_size] = (uint8_t)(val >> 8);
    bc->line_numbers[bc->code_size] = line;
    bc->code_size++;
    bc->code[bc->code_size] = (uint8_t)(val & 0xFF);
    bc->line_numbers[bc->code_size] = line;
    bc->code_size++;
}

void bc_write_u32(Bytecode* bc, uint32_t val, int line) {
    ensure_code_capacity(bc, 4);
    bc->code[bc->code_size + 0] = (uint8_t)(val >> 24); bc->line_numbers[bc->code_size + 0] = line; bc->code_size++;
    bc->code[bc->code_size + 0] = (uint8_t)(val >> 16); bc->line_numbers[bc->code_size + 0] = line; bc->code_size++;
    bc->code[bc->code_size + 0] = (uint8_t)(val >>  8); bc->line_numbers[bc->code_size + 0] = line; bc->code_size++;
    bc->code[bc->code_size + 0] = (uint8_t)(val >>  0); bc->line_numbers[bc->code_size + 0] = line; bc->code_size++;
}

size_t bc_current_offset(Bytecode* bc) {
    return bc->code_size;
}

static void ensure_const_capacity(Bytecode* bc) {
    if (bc->const_capacity > bc->const_count) return;

    size_t old_capacity = bc->const_capacity;
    bc->const_capacity = GROW_CAPACITY(old_capacity);
    bc->constants = GROW_ARRAY(Constant, bc->constants, old_capacity, bc->const_capacity);
}

int bc_add_int(Bytecode* bc, int64_t val) {
    ensure_const_capacity(bc);

    for (size_t i = 0; i < bc->const_count; i++) {
        if (bc->constants[i].type == CONST_INT && bc->constants[i].int_val == val) {
            return (int)i;
        }
    }

    Constant* c = &bc->constants[bc->const_count++];
    c->type = CONST_INT;
    c->int_val = val;
    c->gc_header = 0;  // GC init
    return (int)(bc->const_count - 1);
}

int bc_add_float(Bytecode* bc, double val) {
    if (bc->const_count >= bc->const_capacity) {
        size_t old = bc->const_capacity;
        size_t new_cap = old < 8 ? 8 : old * 2;
        bc->constants = GROW_ARRAY(Constant, bc->constants, old, new_cap);
        bc->const_capacity = new_cap;
    }

    int idx = (int)bc->const_count++;
    Constant* c = &bc->constants[idx];
    c->type = CONST_FLOAT;
    c->float_val = val;
    c->gc_header = 0;
    return idx;
}

int bc_add_string(Bytecode* bc, const char* str) {
    ensure_const_capacity(bc);

    for (size_t i = 0; i < bc->const_count; i++) {
        if (bc->constants[i].type == CONST_STRING &&
            strcmp(bc->constants[i].str_val, str) == 0) {
            return (int)i;
        }
    }

    size_t len = strlen(str);
    Constant* c = &bc->constants[bc->const_count++];
    c->type = CONST_STRING;
    c->str_val = (char*)allocate(len + 1);
    strcpy(c->str_val, str);
    c->gc_header = 0;
    return (int)(bc->const_count - 1);
}

int bc_add_closure(Bytecode* bc, uint32_t func_idx, uint16_t n_upvalues) {
    ensure_const_capacity(bc);

    Constant* c = &bc->constants[bc->const_count++];
    c->type = CONST_CLOSURE;
    c->closure.func_idx = func_idx;
    c->closure.upvalue_count = n_upvalues;
    c->gc_header = 0;  // GC traceable
    return (int)(bc->const_count - 1);
}

int bc_add_class(Bytecode* bc, uint32_t class_idx) {
    ensure_const_capacity(bc);
    Constant* c = &bc->constants[bc->const_count++];
    c->type = CONST_CLASS;
    c->class_ref.class_idx = class_idx;
    c->gc_header = 0;
    return (int)(bc->const_count - 1);
}

static void ensure_func_capacity(Bytecode* bc) {
    if (bc->func_capacity > bc->func_count) return;
    size_t old = bc->func_capacity;
    bc->func_capacity = GROW_CAPACITY(old);
    bc->functions = GROW_ARRAY(Function, bc->functions, old, bc->func_capacity);
}

uint32_t bc_start_function(Bytecode* bc, const char* name, uint16_t arity,
                           uint16_t max_locals, uint16_t max_stack) {
    ensure_func_capacity(bc);

    uint32_t idx = (uint32_t)bc->func_count++;
    Function* fn = &bc->functions[idx];

    size_t name_len = strlen(name);
    fn->name = (char*)allocate(name_len + 1);
    memcpy(fn->name, name, name_len + 1);

    fn->arity = arity;
    fn->code_start = (uint32_t)bc->code_size;
    fn->code_end   = fn->code_start;
    fn->max_locals = max_locals;
    fn->max_stack  = max_stack;
    fn->n_upvalues = 0;
    fn->upvalues   = NULL;
    fn->jit_flags  = 0;
    fn->checksum   = 0;

    return idx;
}

void bc_add_upvalue(Bytecode* bc, uint32_t func_idx,
                    uint16_t location, bool is_local) {
    Function* fn = &bc->functions[func_idx];
    size_t old = fn->n_upvalues;
    size_t newcount = old + 1;
    fn->upvalues = GROW_ARRAY(UpvalueInfo, fn->upvalues, old, newcount);

    UpvalueInfo* uv = &fn->upvalues[old];
    uv->location  = location;
    uv->is_local  = is_local;
    uv->gc_header = 0;

    fn->n_upvalues = (uint16_t)newcount;
}

void bc_end_function(Bytecode* bc, uint32_t func_idx) {
    Function* fn = &bc->functions[func_idx];
    fn->code_end = (uint32_t)bc->code_size;
    // Позже сюда можно добавить вычисление checksum для JIT
}

static void ensure_global_capacity(Bytecode* bc) {
    if (bc->global_capacity > bc->global_count) return;
    size_t old = bc->global_capacity;
    size_t new_cap = old < 8 ? 8 : old * 2;
    bc->globals = GROW_ARRAY(char*, bc->globals, old, new_cap);
    bc->global_const_indices = GROW_ARRAY(uint16_t, bc->global_const_indices, old, new_cap);
    bc->global_capacity = new_cap;
}

int bc_resolve_global(Bytecode* bc, const char* name) {
    for (size_t i = 0; i < bc->global_count; i++) {
        if (strcmp(bc->globals[i], name) == 0) {
            return (int)i;
        }
    }
    return -1;
}

int bc_define_global(Bytecode* bc, const char* name, int const_idx) {
    ensure_global_capacity(bc);

    size_t len = strlen(name);
    char* copy = (char*)allocate(len + 1);
    memcpy(copy, name, len + 1);

    size_t idx = bc->global_count++;
    bc->globals[idx] = copy;
    bc->global_const_indices[idx] = (uint16_t)(const_idx >= 0 ? const_idx : 0);
    return (int)idx;
}

static void ensure_class_capacity(Bytecode* bc) {
    if (bc->class_capacity > bc->class_count) return;
    size_t old = bc->class_capacity;
    bc->class_capacity = GROW_CAPACITY(old);
    bc->classes = GROW_ARRAY(ClassTemplate, bc->classes, old, bc->class_capacity);
}

uint32_t bc_define_class(Bytecode* bc, const char* name,
                         uint16_t n_fields, uint16_t n_methods) {
    ensure_class_capacity(bc);

    uint32_t idx = (uint32_t)bc->class_count++;
    ClassTemplate* ct = &bc->classes[idx];

    ct->n_fields = n_fields;
    ct->n_methods = n_methods;

    if (n_fields > 0) {
        ct->field_names = ALLOCATE(char*, n_fields);
        ct->field_indices = ALLOCATE(uint16_t, n_fields);
        for (uint16_t i = 0; i < n_fields; i++) {
            ct->field_names[i] = NULL;
            ct->field_indices[i] = 0;
        }
    } else {
        ct->field_names = NULL;
        ct->field_indices = NULL;
    }

    if (n_methods > 0) {
        ct->method_names = ALLOCATE(char*, n_methods);
        for (uint16_t i = 0; i < n_methods; i++) {
            ct->method_names[i] = NULL;
        }
    } else {
        ct->method_names = NULL;
    }

    int name_const = bc_add_string(bc, name);
    (void)name_const;

    return idx;
}

void bc_set_field(Bytecode* bc, uint32_t class_idx,
                  uint16_t field_idx, const char* name, int const_idx) {
    ClassTemplate* ct = &bc->classes[class_idx];
    if (field_idx >= ct->n_fields) return;

    size_t len = strlen(name);
    ct->field_names[field_idx] = (char*)allocate(len + 1);
    memcpy(ct->field_names[field_idx], name, len + 1);
    ct->field_indices[field_idx] = (uint16_t)const_idx;
}

void bc_set_method(Bytecode* bc, uint32_t class_idx,
                   uint16_t method_idx, const char* name) {
    ClassTemplate* ct = &bc->classes[class_idx];
    if (method_idx >= ct->n_methods) return;

    size_t len = strlen(name);
    ct->method_names[method_idx] = (char*)allocate(len + 1);
    memcpy(ct->method_names[method_idx], name, len + 1);
}

void bc_disassemble(Bytecode* bc, const char* name) {
    printf("== Bytecode dump: %s ==\n", name ? name : "<module>");
    printf("code_size = %zu, consts = %zu, funcs = %zu, classes = %zu\n",
           bc->code_size, bc->const_count, bc->func_count, bc->class_count);

    printf("-- Constants --\n");
    for (size_t i = 0; i < bc->const_count; i++) {
        Constant* c = &bc->constants[i];
        printf("%04zu: ", i);
        switch (c->type) {
            case CONST_INT:    printf("INT %lld\n", (long long)c->int_val); break;
            case CONST_FLOAT:  printf("FLOAT %g\n", c->float_val); break;
            case CONST_STRING: printf("STRING \"%s\"\n", c->str_val); break;
            case CONST_CLOSURE:
                printf("CLOSURE func=%u, n_upvalues=%u\n",
                       c->closure.func_idx, c->closure.upvalue_count);
                break;
            case CONST_CLASS:
                printf("CLASS idx=%u\n", c->class_ref.class_idx);
                break;
            case CONST_NATIVE_FN:
                printf("NATIVE_FN ptr=%p\n", c->native_ptr);
                break;
            default:
                printf("UNKNOWN\n");
                break;
        }
    }

    printf("-- Functions --\n");
    for (size_t i = 0; i < bc->func_count; i++) {
        Function* fn = &bc->functions[i];
        printf("func %zu: %s [code %u..%u) locals=%u stack=%u upvalues=%u\n",
               i, fn->name, fn->code_start, fn->code_end,
               fn->max_locals, fn->max_stack, fn->n_upvalues);
        for (uint16_t u = 0; u < fn->n_upvalues; u++) {
            UpvalueInfo* uv = &fn->upvalues[u];
            printf("  upvalue %u: %s %u\n", u,
                   uv->is_local ? "local" : "upvalue", uv->location);
        }
    }

    printf("-- Classes --\n");
    for (size_t i = 0; i < bc->class_count; i++) {
        ClassTemplate* ct = &bc->classes[i];
        printf("class %zu: fields=%u methods=%u\n",
               i, ct->n_fields, ct->n_methods);
        for (uint16_t f = 0; f < ct->n_fields; f++) {
            printf("  field %u: %s (const=%u)\n",
                   f, ct->field_names[f] ? ct->field_names[f] : "<anon>",
                   ct->field_indices[f]);
        }
        for (uint16_t m = 0; m < ct->n_methods; m++) {
            printf("  method %u: %s\n",
                   m, ct->method_names[m] ? ct->method_names[m] : "<anon>");
        }
    }

    printf("-- Code --\n");
    size_t ip = 0;
    while (ip < bc->code_size) {
        uint8_t opcode = bc->code[ip];
        int line = bc->line_numbers ? bc->line_numbers[ip] : -1;
        printf("%04zu L%04d  %02X  ", ip, line, opcode);

        OpCode op = (OpCode)opcode;
        uint8_t len = opcode_operand_length[opcode];

        for (uint8_t i = 1; i <= len && ip + i < bc->code_size; i++) {
            printf("%02X ", bc->code[ip + i]);
        }
        for (uint8_t i = len; i < 4; i++) printf("   ");

        switch (op) {
            case OP_NOP:  printf("NOP"); break;
            case OP_HALT: printf("HALT"); break;
            case OP_ADD:  printf("ADD"); break;
            case OP_SUB:  printf("SUB"); break;
            case OP_MUL:  printf("MUL"); break;
            case OP_DIV:  printf("DIV"); break;
            case OP_MOD:  printf("MOD"); break;
            case OP_NEG:  printf("NEG"); break;
            case OP_NOT:  printf("NOT"); break;
            case OP_DUP:  printf("DUP"); break;
            case OP_POP:  printf("POP"); break;
            case OP_EQ:   printf("EQ"); break;
            case OP_NEQ:  printf("NEQ"); break;
            case OP_LT:   printf("LT"); break;
            case OP_LE:   printf("LE"); break;
            case OP_GT:   printf("GT"); break;
            case OP_GE:   printf("GE"); break;
            case OP_AND:  printf("AND"); break;
            case OP_OR:   printf("OR"); break;

            case OP_LOAD_CONST_U16: {
                uint16_t idx = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("LOAD_CONST %u", idx);
                if (idx < bc->const_count && bc->constants[idx].type == CONST_STRING) {
                    printf(" \"%s\"", bc->constants[idx].str_val);
                }
                break;
            }

            case OP_LOAD_LOCAL_U8: {
                uint8_t idx = bc->code[ip+1];
                printf("LOAD_LOCAL %u", idx);
                break;
            }
            case OP_STORE_LOCAL_U8: {
                uint8_t idx = bc->code[ip+1];
                printf("STORE_LOCAL %u", idx);
                break;
            }
            case OP_LOAD_UPVALUE_U8: {
                uint8_t idx = bc->code[ip+1];
                printf("LOAD_UPVALUE %u", idx);
                break;
            }
            case OP_STORE_UPVALUE_U8: {
                uint8_t idx = bc->code[ip+1];
                printf("STORE_UPVALUE %u", idx);
                break;
            }

            case OP_JUMP_U16: {
                uint16_t off = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("JUMP %d -> %zu", (int16_t)off, ip + 3 + (int16_t)off);
                break;
            }
            case OP_JUMP_IF_FALSE_U16: {
                uint16_t off = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("JUMP_IF_FALSE %d -> %zu", (int16_t)off, ip + 3 + (int16_t)off);
                break;
            }
            case OP_JUMP_IF_TRUE_U16: {
                uint16_t off = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("JUMP_IF_TRUE %d -> %zu", (int16_t)off, ip + 3 + (int16_t)off);
                break;
            }

            case OP_CALL_U8: {
                uint8_t argc = bc->code[ip+1];
                printf("CALL %u", argc);
                break;
            }
            case OP_CALL_U16: {
                uint16_t argc = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("CALL %u", argc);
                break;
            }
            case OP_RETURN:     printf("RETURN"); break;
            case OP_RETURN_NIL: printf("RETURN_NIL"); break;

            case OP_NEW_CLOSURE: {
                printf("NEW_CLOSURE");
                break;
            }
            case OP_NEW_CLASS: {
                uint16_t cidx = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("NEW_CLASS %u", cidx);
                break;
            }
            case OP_LOAD_FIELD_U8: {
                uint8_t fidx = bc->code[ip+1];
                printf("LOAD_FIELD %u", fidx);
                break;
            }
            case OP_STORE_FIELD_U8: {
                uint8_t fidx = bc->code[ip+1];
                printf("STORE_FIELD %u", fidx);
                break;
            }
            case OP_CALL_METHOD_U8: {
                uint8_t midx = bc->code[ip+1];
                printf("CALL_METHOD %u", midx);
                break;
            }

            case OP_ARRAY_NEW_U8: {
                uint8_t sz = bc->code[ip+1];
                printf("ARRAY_NEW %u", sz);
                break;
            }
            case OP_ARRAY_GET: printf("ARRAY_GET"); break;
            case OP_ARRAY_SET: printf("ARRAY_SET"); break;
            case OP_ARRAY_LEN: printf("ARRAY_LEN"); break;

            case OP_LOAD_GLOBAL_U16: {
                uint16_t idx = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("LOAD_GLOBAL %u", idx);
                break;
            }
            case OP_STORE_GLOBAL_U16: {
                uint16_t idx = (uint16_t)((bc->code[ip+1] << 8) | bc->code[ip+2]);
                printf("STORE_GLOBAL %u", idx);
                break;
            }

            case OP_PRINT: printf("PRINT"); break;
            case OP_BREAK: printf("BREAK"); break;

            default:
                printf("UNKNOWN_%02X", opcode);
                break;
        }

        printf("\n");
        ip += 1 + len;
    }
}

int bytecode_save(Bytecode* bc, const char* path) {
    FILE* f = fopen(path, "wb");
    if (!f) return 0;

    uint32_t magic = 0x4D595448; // "MYTH"
    uint16_t version = 0x0002;

    fwrite(&magic, sizeof(uint32_t), 1, f);
    fwrite(&version, sizeof(uint16_t), 1, f);

    uint32_t code_size = (uint32_t)bc->code_size;
    fwrite(&code_size, sizeof(uint32_t), 1, f);
    fwrite(bc->code, sizeof(uint8_t), code_size, f);
    fwrite(bc->line_numbers, sizeof(int), code_size, f);

    uint32_t const_count = (uint32_t)bc->const_count;
    fwrite(&const_count, sizeof(uint32_t), 1, f);
    for (uint32_t i = 0; i < const_count; i++) {
        Constant* c = &bc->constants[i];
        uint8_t type = (uint8_t)c->type;
        fwrite(&type, sizeof(uint8_t), 1, f);
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
            case CONST_CLOSURE:
                fwrite(&c->closure.func_idx, sizeof(uint32_t), 1, f);
                fwrite(&c->closure.upvalue_count, sizeof(uint16_t), 1, f);
                break;
            case CONST_CLASS:
                fwrite(&c->class_ref.class_idx, sizeof(uint32_t), 1, f);
                break;
            case CONST_NATIVE_FN:
                // Нативки не сериализуем; пишем ноль, потом при load = NULL
                // чтобы не поломать формат.
                break;
        }
    }

    uint32_t func_count = (uint32_t)bc->func_count;
    fwrite(&func_count, sizeof(uint32_t), 1, f);
    for (uint32_t i = 0; i < func_count; i++) {
        Function* fn = &bc->functions[i];
        uint32_t name_len = (uint32_t)strlen(fn->name);
        fwrite(&name_len, sizeof(uint32_t), 1, f);
        fwrite(fn->name, sizeof(char), name_len, f);

        fwrite(&fn->arity,      sizeof(uint16_t), 1, f);
        fwrite(&fn->code_start, sizeof(uint32_t), 1, f);
        fwrite(&fn->code_end, sizeof(uint32_t), 1, f);
        fwrite(&fn->max_locals, sizeof(uint16_t), 1, f);
        fwrite(&fn->max_stack, sizeof(uint16_t), 1, f);
        fwrite(&fn->n_upvalues, sizeof(uint16_t), 1, f);

        for (uint16_t u = 0; u < fn->n_upvalues; u++) {
            UpvalueInfo* uv = &fn->upvalues[u];
            fwrite(&uv->location, sizeof(uint16_t), 1, f);
            uint8_t is_local = uv->is_local ? 1 : 0;
            fwrite(&is_local, sizeof(uint8_t), 1, f);
        }
    }

    uint32_t class_count = (uint32_t)bc->class_count;
    fwrite(&class_count, sizeof(uint32_t), 1, f);
    for (uint32_t i = 0; i < class_count; i++) {
        ClassTemplate* ct = &bc->classes[i];
        fwrite(&ct->n_fields, sizeof(uint16_t), 1, f);
        fwrite(&ct->n_methods, sizeof(uint16_t), 1, f);

        for (uint16_t fi = 0; fi < ct->n_fields; fi++) {
            uint32_t name_len = ct->field_names[fi]
                ? (uint32_t)strlen(ct->field_names[fi]) : 0;
            fwrite(&name_len, sizeof(uint32_t), 1, f);
            if (name_len > 0) {
                fwrite(ct->field_names[fi], sizeof(char), name_len, f);
            }
            fwrite(&ct->field_indices[fi], sizeof(uint16_t), 1, f);
        }

        for (uint16_t mi = 0; mi < ct->n_methods; mi++) {
            uint32_t name_len = ct->method_names[mi]
                ? (uint32_t)strlen(ct->method_names[mi]) : 0;
            fwrite(&name_len, sizeof(uint32_t), 1, f);
            if (name_len > 0) {
                fwrite(ct->method_names[mi], sizeof(char), name_len, f);
            }
        }
    }

    fwrite(&bc->main_closure_idx, sizeof(uint32_t), 1, f);

    fclose(f);
    return 1;
}

Bytecode* bytecode_load(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) return NULL;

    uint32_t magic;
    uint16_t version;
    if (fread(&magic, sizeof(uint32_t), 1, f) != 1 || magic != 0x4D595448) {
        fclose(f);
        return NULL;
    }
    if (fread(&version, sizeof(uint16_t), 1, f) != 1 || version != 0x0002) {
        fclose(f);
        return NULL;
    }

    Bytecode* bc = bytecode_new();

    // code
    uint32_t code_size;
    fread(&code_size, sizeof(uint32_t), 1, f);
    bc->code_size = bc->code_capacity = code_size;
    bc->code = ALLOCATE(uint8_t, code_size);
    bc->line_numbers = ALLOCATE(int, code_size);
    fread(bc->code, sizeof(uint8_t), code_size, f);
    fread(bc->line_numbers, sizeof(int), code_size, f);

    // constants
    uint32_t const_count;
    fread(&const_count, sizeof(uint32_t), 1, f);
    bc->const_count = bc->const_capacity = const_count;
    bc->constants = ALLOCATE(Constant, const_count);
    for (uint32_t i = 0; i < const_count; i++) {
        uint8_t type;
        fread(&type, sizeof(uint8_t), 1, f);
        Constant* c = &bc->constants[i];
        memset(c, 0, sizeof(Constant));
        c->type = (ConstType)type;
        switch (c->type) {
            case CONST_INT:
                fread(&c->int_val, sizeof(int64_t), 1, f);
                break;
            case CONST_FLOAT:
                fread(&c->float_val, sizeof(double), 1, f);
                break;
            case CONST_STRING: {
                uint32_t len;
                fread(&len, sizeof(uint32_t), 1, f);
                c->str_val = (char*)allocate(len + 1);
                fread(c->str_val, sizeof(char), len, f);
                c->str_val[len] = '\0';
                break;
            }
            case CONST_CLOSURE:
                fread(&c->closure.func_idx, sizeof(uint32_t), 1, f);
                fread(&c->closure.upvalue_count, sizeof(uint16_t), 1, f);
                break;
            case CONST_CLASS:
                fread(&c->class_ref.class_idx, sizeof(uint32_t), 1, f);
                break;
            case CONST_NATIVE_FN:
                c->native_ptr = NULL;
                break;
        }
        c->gc_header = 0;
    }

    uint32_t func_count;
    fread(&func_count, sizeof(uint32_t), 1, f);
    bc->func_count = bc->func_capacity = func_count;
    bc->functions = ALLOCATE(Function, func_count);
    for (uint32_t i = 0; i < func_count; i++) {
        Function* fn = &bc->functions[i];
        memset(fn, 0, sizeof(Function));

        uint32_t name_len;
        fread(&name_len, sizeof(uint32_t), 1, f);
        fn->name = (char*)allocate(name_len + 1);
        fread(fn->name, sizeof(char), name_len, f);
        fn->name[name_len] = '\0';

        fread(&fn->arity,      sizeof(uint16_t), 1, f);
        fread(&fn->code_start, sizeof(uint32_t), 1, f);
        fread(&fn->code_end, sizeof(uint32_t), 1, f);
        fread(&fn->max_locals, sizeof(uint16_t), 1, f);
        fread(&fn->max_stack, sizeof(uint16_t), 1, f);
        fread(&fn->n_upvalues, sizeof(uint16_t), 1, f);

        if (fn->n_upvalues > 0) {
            fn->upvalues = ALLOCATE(UpvalueInfo, fn->n_upvalues);
            for (uint16_t u = 0; u < fn->n_upvalues; u++) {
                UpvalueInfo* uv = &fn->upvalues[u];
                fread(&uv->location, sizeof(uint16_t), 1, f);
                uint8_t is_local;
                fread(&is_local, sizeof(uint8_t), 1, f);
                uv->is_local = is_local != 0;
                uv->gc_header = 0;
            }
        } else {
            fn->upvalues = NULL;
        }

        fn->jit_flags = 0;
        fn->checksum = 0;
    }

    uint32_t class_count;
    fread(&class_count, sizeof(uint32_t), 1, f);
    bc->class_count = bc->class_capacity = class_count;
    bc->classes = ALLOCATE(ClassTemplate, class_count);
    for (uint32_t i = 0; i < class_count; i++) {
        ClassTemplate* ct = &bc->classes[i];
        memset(ct, 0, sizeof(ClassTemplate));

        fread(&ct->n_fields, sizeof(uint16_t), 1, f);
        fread(&ct->n_methods, sizeof(uint16_t), 1, f);

        if (ct->n_fields > 0) {
            ct->field_names = ALLOCATE(char*, ct->n_fields);
            ct->field_indices = ALLOCATE(uint16_t, ct->n_fields);
            for (uint16_t fi = 0; fi < ct->n_fields; fi++) {
                uint32_t name_len;
                fread(&name_len, sizeof(uint32_t), 1, f);
                if (name_len > 0) {
                    ct->field_names[fi] = (char*)allocate(name_len + 1);
                    fread(ct->field_names[fi], sizeof(char), name_len, f);
                    ct->field_names[fi][name_len] = '\0';
                } else {
                    ct->field_names[fi] = NULL;
                }
                fread(&ct->field_indices[fi], sizeof(uint16_t), 1, f);
            }
        } else {
            ct->field_names = NULL;
            ct->field_indices = NULL;
        }

        if (ct->n_methods > 0) {
            ct->method_names = ALLOCATE(char*, ct->n_methods);
            for (uint16_t mi = 0; mi < ct->n_methods; mi++) {
                uint32_t name_len;
                fread(&name_len, sizeof(uint32_t), 1, f);
                if (name_len > 0) {
                    ct->method_names[mi] = (char*)allocate(name_len + 1);
                    fread(ct->method_names[mi], sizeof(char), name_len, f);
                    ct->method_names[mi][name_len] = '\0';
                } else {
                    ct->method_names[mi] = NULL;
                }
            }
        } else {
            ct->method_names = NULL;
        }
    }

    fread(&bc->main_closure_idx, sizeof(uint32_t), 1, f);

    fclose(f);
    return bc;
}