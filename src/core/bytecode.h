#ifndef MYTHON_BYTECODE_H
#define MYTHON_BYTECODE_H

#include <stdint.h>
#include <stddef.h>

typedef enum {
    // Нет операнда (1 байт)
    OP_NOP = 0x00,
    OP_HALT = 0x01,
    OP_ADD = 0x02,
    OP_SUB = 0x03,
    OP_MUL = 0x04,
    OP_DIV = 0x05,
    OP_MOD = 0x06,
    OP_NEG = 0x07,
    OP_NOT = 0x08,
    OP_EQ = 0x09,
    OP_NEQ = 0x0A,
    OP_LT = 0x0B,
    OP_LE = 0x0C,
    OP_GT = 0x0D,
    OP_GE = 0x0E,
    OP_AND = 0x0F,
    OP_OR = 0x10,

    // 1-байтовый операнд (2 байта)
    OP_PUSH_I8 = 0x20,
    OP_PUSH_NIL = 0x21,
    OP_PUSH_TRUE = 0x22,
    OP_PUSH_FALSE = 0x23,
    OP_STORE_LOCAL_8 = 0x24,
    OP_LOAD_LOCAL_8 = 0x25,
    OP_JUMP_8 = 0x26,
    OP_JUMP_IF_TRUE_8 = 0x27,
    OP_JUMP_IF_FALSE_8 = 0x28,
    OP_POP = 0x29,

    OP_PUSH_I16 = 0x30,
    OP_STORE_LOCAL_16 = 0x31,
    OP_LOAD_LOCAL_16 = 0x32,
    OP_JUMP_16 = 0x33,
    OP_JUMP_IF_TRUE_16 = 0x34,
    OP_JUMP_IF_FALSE_16 = 0x35,
    OP_CALL_8 = 0x36,

    // 4-байтовый операнд (5 байт)
    OP_PUSH_I32 = 0x40,
    OP_PUSH_F32 = 0x41,
    OP_JUMP_32 = 0x42,
    OP_CALL_16 = 0x43,
    OP_LOAD_CONST = 0x44,
    OP_LOAD_GLOBAL = 0x45,

    // Массивы
    OP_ARRAY_NEW = 0x50,
    OP_ARRAY_GET = 0x51,
    OP_ARRAY_SET = 0x52,
    OP_ARRAY_LEN = 0x53,

    // Функции
    OP_CALL_NATIVE = 0x60,
    OP_RETURN = 0x61,
    OP_RETURN_NIL = 0x62,

    // Встроенные функции
    OP_PRINT = 0x70,
    OP_DUP = 0x71,

    // Для отладки
    OP_BREAK = 0xFF,
} OpCode;

extern const uint8_t opcode_operand_length[256];

typedef struct Constant {
    enum {
        CONST_INT,
        CONST_FLOAT,
        CONST_STRING,
        CONST_FUNCTION,
        CONST_NATIVE_FN,
    } type;

    union {
        int64_t int_val;
        double float_val;
        char* str_val;
        void* ptr_val;
    };
} Constant;

typedef struct {
    uint8_t* code;
    size_t capacity;
    size_t length;

    Constant* constants;
    size_t const_capacity;
    size_t const_count;

    int* line_numbers;
    char** source_lines;

    struct {
        char** names;
        size_t* addresses;
        int* local_counts;
        int count;
    } functions;
} Bytecode;

Bytecode* bytecode_new(void);
void bytecode_free(Bytecode* bc);

void bc_write_byte(Bytecode* bc, uint8_t byte, int line);
void bc_write_op(Bytecode* bc, OpCode op, int line);
void bc_write_i8(Bytecode* bc, int8_t value, int line);
void bc_write_i16(Bytecode* bc, int16_t value, int line);
void bc_write_i32(Bytecode* bc, int32_t value, int line);
void bc_write_u8(Bytecode* bc, uint8_t value, int line);
void bc_write_u16(Bytecode* bc, uint16_t value, int line);
void bc_write_u32(Bytecode* bc, uint32_t value, int line);

int bc_add_constant_int(Bytecode* bc, int64_t value);
int bc_add_constant_float(Bytecode* bc, double value);
int bc_add_constant_string(Bytecode* bc, const char* str);
int bc_add_constant_ptr(Bytecode* bc, void* ptr, int type);

void bc_add_function(Bytecode* bc, const char* name, size_t address, int locals);

size_t bc_get_current_address(Bytecode* bc);
void bc_patch_i16(Bytecode* bc, size_t address, int16_t value);
void bc_patch_i32(Bytecode* bc, size_t address, int32_t value);

void bc_disassemble(const Bytecode* bc);
void bc_disassemble_instruction(const Bytecode* bc, size_t* ip);

int bc_save_to_file(const Bytecode* bc, const char* filename);
Bytecode* bc_load_from_file(const char* filename);

#endif
