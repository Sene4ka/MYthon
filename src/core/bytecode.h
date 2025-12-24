#ifndef MYTHON_BYTECODE_H
#define MYTHON_BYTECODE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef enum {
    // Stack ops
    OP_NOP = 0x00, OP_HALT = 0x01,
    OP_ADD = 0x02, OP_SUB = 0x03, OP_MUL = 0x04, OP_DIV = 0x05, OP_MOD = 0x06,
    OP_NEG = 0x07, OP_NOT = 0x08, OP_DUP = 0x09, OP_POP = 0x0A,
    OP_EQ = 0x0B, OP_NEQ = 0x0C, OP_LT = 0x0D, OP_LE = 0x0E, OP_GT = 0x0F, OP_GE = 0x10,
    OP_AND = 0x11, OP_OR = 0x12,

    // Constants
    OP_LOAD_CONST_U16 = 0x20,

    // Locals/Upvalues
    OP_LOAD_LOCAL_U8 = 0x30, OP_STORE_LOCAL_U8 = 0x31,
    OP_LOAD_UPVALUE_U8 = 0x32, OP_STORE_UPVALUE_U8 = 0x33,

    // Control flow
    OP_JUMP_U16 = 0x40, OP_JUMP_IF_FALSE_U16 = 0x41, OP_JUMP_IF_TRUE_U16 = 0x42,

    // Calls
    OP_CALL_U8 = 0x60, OP_CALL_U16 = 0x61,
    OP_RETURN = 0x62, OP_RETURN_NIL = 0x63,

    // Classes/Objects
    OP_NEW_CLOSURE = 0x80,
    OP_NEW_CLASS = 0x81,
    OP_LOAD_FIELD_U8 = 0x82,
    OP_STORE_FIELD_U8 = 0x83,
    OP_CALL_METHOD_U8 = 0x84,

    // Arrays
    OP_ARRAY_NEW_U8 = 0xA0, OP_ARRAY_GET = 0xA1, OP_ARRAY_SET = 0xA2, OP_ARRAY_LEN = 0xA3,

    // Globals
    OP_LOAD_GLOBAL_U16 = 0xB0, OP_STORE_GLOBAL_U16 = 0xB1,

    // Debug/GC
    OP_PRINT = 0xF0, OP_BREAK = 0xFF
} OpCode;

extern const uint8_t opcode_operand_length[256];

typedef enum {
    CONST_INT, CONST_FLOAT, CONST_STRING,
    CONST_CLOSURE, CONST_CLASS, CONST_NATIVE_FN
} ConstType;

typedef struct {
    ConstType type;
    union {
        int64_t int_val;
        double float_val;
        char* str_val;
        struct { uint32_t func_idx; uint16_t upvalue_count; } closure;
        struct { uint32_t class_idx; } class_ref;
        void* native_ptr;
    };

    uint32_t gc_header;
} Constant;

typedef struct {
    uint16_t location;
    bool is_local;
    uint32_t gc_header;
} UpvalueInfo;

typedef struct {
    char* name;
    uint32_t code_start;
    uint32_t code_end;
    uint16_t arity;
    uint16_t max_locals;
    uint16_t max_stack;
    uint16_t n_upvalues;
    UpvalueInfo* upvalues;
    uint32_t jit_flags;
    uint64_t checksum;
} Function;

typedef struct {
    char** field_names;
    char** method_names;
    uint16_t* field_indices;
    uint16_t n_fields;
    uint16_t n_methods;
} ClassTemplate;

typedef struct {
    uint8_t* code;
    size_t code_capacity;
    size_t code_size;

    Constant* constants;
    size_t const_capacity;
    size_t const_count;

    Function* functions;
    size_t func_capacity;
    size_t func_count;

    char** globals;
    uint16_t* global_const_indices;
    size_t global_count;
    size_t global_capacity;

    ClassTemplate* classes;
    size_t class_capacity;
    size_t class_count;

    uint32_t main_closure_idx;

    uint32_t* gc_roots;
    size_t gc_roots_count;

    int* line_numbers;
} Bytecode;

Bytecode* bytecode_new(void);
void bytecode_free(Bytecode* bc);

void bc_write_op(Bytecode* bc, OpCode op, int line);
void bc_write_u8(Bytecode* bc, uint8_t val, int line);
void bc_write_u16(Bytecode* bc, uint16_t val, int line);
void bc_write_u32(Bytecode* bc, uint32_t val, int line);

int bc_add_int(Bytecode* bc, int64_t val);
int bc_add_float(Bytecode* bc, double val);
int bc_add_string(Bytecode* bc, const char* str);
int bc_add_closure(Bytecode* bc, uint32_t func_idx, uint16_t n_upvalues);

uint32_t bc_start_function(Bytecode* bc, const char* name, uint16_t arity, uint16_t max_locals, uint16_t max_stack);
void bc_add_upvalue(Bytecode* bc, uint32_t func_idx, uint16_t location, bool is_local);
void bc_end_function(Bytecode* bc, uint32_t func_idx);

int bc_resolve_global(Bytecode* bc, const char* name);
int bc_define_global(Bytecode* bc, const char* name, int const_idx);

uint32_t bc_define_class(Bytecode* bc, const char* name, uint16_t n_fields, uint16_t n_methods);
void bc_set_field(Bytecode* bc, uint32_t class_idx, uint16_t field_idx, const char* name, int const_idx);
void bc_set_method(Bytecode* bc, uint32_t class_idx, uint16_t method_idx, const char* name);

size_t bc_current_offset(Bytecode* bc);
void bc_disassemble(Bytecode* bc, const char* name);
int bytecode_save(Bytecode* bc, const char* path);
Bytecode* bytecode_load(const char* path);

#endif

