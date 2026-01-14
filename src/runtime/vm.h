#ifndef MYTHON_VM_H
#define MYTHON_VM_H

#include "bytecode.h"
#include <stdint.h>
#include "jit.h"

typedef enum {
    VAL_NIL,
    VAL_BOOL,
    VAL_INT,
    VAL_FLOAT,
    VAL_STRING,
    VAL_ARRAY,
    VAL_FUNCTION,
    VAL_NATIVE_FN,
    VAL_CLOSURE,
    VAL_UPVALUE
} ValueType;

typedef enum {
    DEBUG_NONE   = 0,
    DEBUG_OPS    = 1,
    DEBUG_STACK  = 2,
    DEBUG_GLOBAL = 3
} DebugLevel;

typedef struct Value Value;
typedef struct Object Object;

struct Value {
    ValueType type;
    uint32_t padding;
    union {
        int boolean;
        int64_t integer;
        double floating;
        Object* object;
    } as;
};

struct Object {
    uint8_t type;
    uint8_t marked;
    Object* next;
};

typedef struct {
    Object obj;
    char* chars;
    int length;
    uint32_t hash;
} StringObject;

typedef struct {
    Object obj;
    Value* items;
    int capacity;
    int count;
} ArrayObject;

typedef struct {
    Object obj;
    char* name;
    int arity;
    int local_count;
    int upvalue_count;
    Bytecode* bytecode;
    int func_index;
} FunctionObject;


typedef struct Upvalue {
    Object obj;
    Value* location;
    Value closed;
    struct Upvalue* next;
} Upvalue;

typedef struct {
    Object obj;
    FunctionObject* function;
    Upvalue** upvalues;
    int upvalue_count;
} ClosureObject;

#define IS_NIL(value)        ((value).type == VAL_NIL)
#define IS_BOOL(value)       ((value).type == VAL_BOOL)
#define IS_INT(value)        ((value).type == VAL_INT)
#define IS_FLOAT(value)      ((value).type == VAL_FLOAT)
#define IS_STRING(value)     ((value).type == VAL_STRING)
#define IS_ARRAY(value)      ((value).type == VAL_ARRAY)
#define IS_FUNCTION(value)   ((value).type == VAL_FUNCTION)
#define IS_NATIVE(value)     ((value).type == VAL_NATIVE_FN)
#define IS_CLOSURE(value)    ((value).type == VAL_CLOSURE)
#define IS_OBJECT(value)     ((value).type >= VAL_STRING)

#define AS_BOOL(value)       ((value).as.boolean)
#define AS_INT(value)        ((value).as.integer)
#define AS_FLOAT(value)      ((value).as.floating)
#define AS_OBJECT(value)     ((value).as.object)
#define AS_STRING(value)     ((StringObject*)AS_OBJECT(value))
#define AS_ARRAY(value)      ((ArrayObject*)AS_OBJECT(value))
#define AS_FUNCTION(value)   ((FunctionObject*)AS_OBJECT(value))
#define AS_NATIVE(value)     ((NativeFunctionObject*)AS_OBJECT(value))
#define AS_CLOSURE(value)    ((ClosureObject*)AS_OBJECT(value))

#define BOOL_VAL(b)          ((Value){VAL_BOOL,   0, {.boolean = (b)}})
#define INT_VAL(i)           ((Value){VAL_INT,    0, {.integer = (i)}})
#define FLOAT_VAL(f)         ((Value){VAL_FLOAT,  0, {.floating = (f)}})
#define NIL_VAL              ((Value){VAL_NIL,    0, {.integer = 0}})
#define OBJECT_VAL(o)        ((Value){((Object*)(o))->type, 0, {.object = (Object*)(o)}})

typedef struct {
    Value* values;
    int capacity;
    int count;
} ValueArray;

typedef struct {
    Bytecode* bytecode;
    uint8_t* ip;
    int slots_offset;
    int slot_count;
    ClosureObject* closure;
} CallFrame;

typedef struct VM {
    Value* stack;
    int stack_size;
    int stack_capacity;
    int sp;

    CallFrame* frames;
    int frame_capacity;
    int frame_count;

    Upvalue* open_upvalues;

    Value* globals;
    int global_capacity;
    int global_count;

    ValueArray constants;

    Object* objects;
    size_t bytes_allocated;
    size_t next_gc;
    int gc_collecting;
    int gc_enabled;

    int error_count;
    const char* error_message;
    int line;

    int debug;
    int debug_gc;
    DebugLevel debug_level;

    JIT* jit;

    int exit_code;
} VM;

typedef Value (*NativeFn)(VM* vm, int arg_count, Value* args);

typedef struct {
    Object obj;
    NativeFn function;
    char* name;
    int arity;
} NativeFunctionObject;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

VM* vm_new(void);
void vm_free(VM* vm);

InterpretResult vm_run(VM* vm, Bytecode* bytecode);

void vm_push(VM* vm, Value value);
Value vm_pop(VM* vm);
Value vm_peek(VM* vm, int distance);

// VM Operation handlers

int vm_op_add(VM* vm);
int vm_op_sub(VM* vm);
int vm_op_mul(VM* vm);
int vm_op_div(VM* vm);
int vm_op_mod(VM* vm);

int vm_op_neg(VM* vm);
int vm_op_not(VM* vm);

int vm_op_dup(VM* vm);

int vm_op_eq(VM* vm);
int vm_op_neq(VM* vm);
int vm_op_lt(VM* vm);
int vm_op_le(VM* vm);
int vm_op_gt(VM* vm);
int vm_op_ge(VM* vm);
int vm_op_and(VM* vm);
int vm_op_or(VM* vm);

int vm_op_load_const_u16(VM* vm, Bytecode* bytecode, CallFrame* frame, uint8_t* ip);

int vm_op_load_local_u8(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_store_local_u8(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_load_upvalue_u8(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_store_upvalue_u8(VM* vm, CallFrame* frame, uint8_t* ip);

int vm_op_jump_u16(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_jump_if_false_u16(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_jump_if_true_u16(VM* vm, CallFrame* frame, uint8_t* ip);

int vm_op_call_u8(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_call_u16(VM* vm, CallFrame* frame, uint8_t* ip);

int vm_op_new_closure(VM* vm, Bytecode* bytecode, CallFrame* frame, uint8_t* ip);

int vm_op_array_new_u8(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_array_get(VM* vm);
int vm_op_array_set(VM* vm);
int vm_op_array_len(VM* vm);

int vm_op_load_global_u16(VM* vm, CallFrame* frame, uint8_t* ip);
int vm_op_store_global_u16(VM* vm, CallFrame* frame, uint8_t* ip);

int vm_op_print(VM* vm);

// Helpers
void vm_push_frame(VM* vm, Bytecode* bytecode, int slot_count);
void vm_push_frame_with_ip(VM* vm, Bytecode* bytecode, int slot_count, uint8_t* ip_start);
void vm_push_frame_with_ip_at(VM* vm, Bytecode* bytecode, int slot_count, uint8_t* ip_start, int slots_offset);
void vm_pop_frame(VM* vm);

Upvalue* vm_capture_upvalue(VM* vm, Value* slot);

void vm_store_global(VM* vm, int index, Value value);
Value vm_load_global(VM* vm, int index);
void vm_store_local(VM* vm, int index, Value value);
Value vm_load_local(VM* vm, int index);

Value vm_call_value(VM* vm, Value callee, int arg_count);
Value vm_call_native(VM* vm, NativeFn function, int arg_count);

void vm_runtime_error(VM* vm, const char* format, ...);
void vm_print_value(Value value);
const char* vm_value_type_name(Value value);
int vm_values_equal(Value a, Value b);
void vm_set_debug(VM* vm, int debug);

StringObject* vm_take_string(VM* vm, char* chars, int length);
StringObject* vm_copy_string(VM* vm, const char* chars, int length);
ArrayObject* vm_new_array(VM* vm, int capacity);
void vm_array_append(VM* vm, ArrayObject* array, Value value);

NativeFunctionObject* vm_new_native_function(VM* vm,
                                             const char* name,
                                             NativeFn function,
                                             int arity);

FunctionObject* vm_new_function(VM* vm,
                                const char* name,
                                int arity,
                                int local_count,
                                int upvalue_count,
                                Bytecode* bytecode,
                                int func_index);

ClosureObject* vm_new_closure(VM* vm, FunctionObject* function);

void vm_close_upvalues(VM* vm, Value* last);

uint16_t read_u16(uint8_t* ip);

Value* vm_get_global(VM* vm, uint16_t index);
void vm_set_global(VM* vm, uint16_t index, Value* value);
int vm_call_function(VM* vm, int arg_count);
void vm_print(VM* vm);
int vm_array_get(VM* vm);
int vm_array_set(VM* vm);
int vm_array_len(VM* vm);

#endif