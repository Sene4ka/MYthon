#ifndef MYTHON_VM_H
#define MYTHON_VM_H

#include "bytecode.h"
#include <stdint.h>

typedef enum {
    VAL_NIL,
    VAL_BOOL,
    VAL_INT,
    VAL_FLOAT,
    VAL_STRING,
    VAL_ARRAY,
    VAL_FUNCTION,
    VAL_NATIVE_FN,
    VAL_CLOSURE
} ValueType;

typedef struct Value Value;
typedef struct Object Object;

struct Value {
    ValueType type;
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
    Bytecode* bytecode;
} FunctionObject;

typedef Value (*NativeFn)(int arg_count, Value* args);

typedef struct {
    Object obj;
    NativeFn function;
    char* name;
    int arity;
} NativeFunctionObject;

typedef struct {
    Object obj;
    FunctionObject* function;
    Value* upvalues;
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

#define BOOL_VAL(b)          ((Value){VAL_BOOL, {.boolean = (b)}})
#define INT_VAL(i)           ((Value){VAL_INT, {.integer = (i)}})
#define FLOAT_VAL(f)         ((Value){VAL_FLOAT, {.floating = (f)}})
#define NIL_VAL              ((Value){VAL_NIL, {.integer = 0}})
#define OBJECT_VAL(obj) ((Value){((Object*)(obj))->type, {.object = (Object*)(obj)}})

typedef struct {
    Value* values;
    int capacity;
    int count;
} ValueArray;

typedef struct {
    Bytecode* bytecode;
    uint8_t* ip;
    Value* slots;
    int slot_count;
} CallFrame;

typedef struct {
    Value* stack;
    int stack_size;
    int stack_capacity;
    int sp;
    
    CallFrame* frames;
    int frame_capacity;
    int frame_count;
    
    Value* globals;
    int global_capacity;
    int global_count;
    
    ValueArray constants;
    
    Object* objects;
    
    int error_count;
    const char* error_message;
    int line;
    
    int exit_code;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

VM* vm_new(void);
void vm_free(VM* vm);

InterpretResult vm_interpret(VM* vm, const char* source);
InterpretResult vm_run(VM* vm, Bytecode* bytecode);

void vm_push(VM* vm, Value value);
Value vm_pop(VM* vm);
Value vm_peek(VM* vm, int distance);

void vm_push_frame(VM* vm, Bytecode* bytecode, int slot_count);
void vm_pop_frame(VM* vm);

void vm_store_global(VM* vm, int index, Value value);
Value vm_load_global(VM* vm, int index);
void vm_store_local(VM* vm, int index, Value value);
Value vm_load_local(VM* vm, int index);

Value vm_call_function(VM* vm, int function_index, int arg_count);
Value vm_call_native(VM* vm, NativeFn function, int arg_count);

void vm_runtime_error(VM* vm, const char* format, ...);

StringObject* vm_take_string(VM* vm, char* chars, int length);
StringObject* vm_copy_string(VM* vm, const char* chars, int length);
ArrayObject* vm_new_array(VM* vm, int capacity);
void vm_array_append(VM* vm, ArrayObject* array, Value value);

int vm_values_equal(Value a, Value b);
void vm_print_value(Value value);

const char* vm_value_type_name(Value value);

NativeFunctionObject* vm_new_native_function(VM* vm, const char* name, NativeFn function, int arity);

#endif