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
    VAL_CLOSURE,
    VAL_CLASS,
    VAL_INSTANCE,
    VAL_UPVALUE,
} ValueType;

typedef enum {
    DEBUG_NONE   = 0,
    DEBUG_OPS    = 1,  // ip + opcode
    DEBUG_STACK  = 2,  // + стек и фреймы
    DEBUG_GLOBAL = 3   // + глобалы и upvalues
} DebugLevel;

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

struct ClassObject;
struct InstanceObject;

typedef struct {
    Object obj;
    char* name;
    int arity;
    int local_count;
    int upvalue_count;
    Bytecode* bytecode;
    int func_index;
} FunctionObject;

typedef Value (*NativeFn)(int arg_count, Value* args);

typedef struct {
    Object obj;
    NativeFn function;
    char* name;
    int arity;
} NativeFunctionObject;

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

typedef struct ClassObject {
    Object obj;
    StringObject* name;

    StringObject** field_names;
    int field_count;

    ClosureObject** methods;
    int method_count;
} ClassObject;

typedef struct InstanceObject {
    Object obj;
    ClassObject* klass;

    Value* fields;
    int field_count;
} InstanceObject;

/* value helpers */
#define IS_NIL(value)        ((value).type == VAL_NIL)
#define IS_BOOL(value)       ((value).type == VAL_BOOL)
#define IS_INT(value)        ((value).type == VAL_INT)
#define IS_FLOAT(value)      ((value).type == VAL_FLOAT)
#define IS_STRING(value)     ((value).type == VAL_STRING)
#define IS_ARRAY(value)      ((value).type == VAL_ARRAY)
#define IS_FUNCTION(value)   ((value).type == VAL_FUNCTION)
#define IS_NATIVE(value)     ((value).type == VAL_NATIVE_FN)
#define IS_CLOSURE(value)    ((value).type == VAL_CLOSURE)
#define IS_CLASS(value)      ((value).type == VAL_CLASS)
#define IS_INSTANCE(value)   ((value).type == VAL_INSTANCE)
#define IS_UPVALUE(value)    ((value).type == VAL_UPVALUE)
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
#define AS_CLASS(value)      ((ClassObject*)AS_OBJECT(value))
#define AS_INSTANCE(value)   ((InstanceObject*)AS_OBJECT(value))
#define AS_UPVALUE(value)    ((Upvalue*)AS_OBJECT(value))

#define BOOL_VAL(b)          ((Value){VAL_BOOL,   {.boolean = (b)}})
#define INT_VAL(i)           ((Value){VAL_INT,    {.integer = (i)}})
#define FLOAT_VAL(f)         ((Value){VAL_FLOAT,  {.floating = (f)}})
#define NIL_VAL              ((Value){VAL_NIL,    {.integer = 0}})
#define OBJECT_VAL(o)        ((Value){((Object*)(o))->type, {.object = (Object*)(o)}})

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
    size_t bytes_allocated;
    size_t next_gc;

    Upvalue* open_upvalues;

    int error_count;
    const char* error_message;
    int line;
    int debug;
    DebugLevel debug_level;
    int exit_code;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

/* VM lifecycle */
VM* vm_new(void);
void vm_free(VM* vm);

/* Interpretation */
InterpretResult vm_interpret(VM* vm, const char* source);
InterpretResult vm_run(VM* vm, Bytecode* bytecode);

/* Stack */
void vm_push(VM* vm, Value value);
Value vm_pop(VM* vm);
Value vm_peek(VM* vm, int distance);

/* Call frames */
void vm_push_frame(VM* vm, Bytecode* bytecode, int slot_count);
void vm_push_frame_with_ip(VM* vm, Bytecode* bytecode, int slot_count, uint8_t* ip_start);
void vm_pop_frame(VM* vm);

/* Globals / locals */
void vm_store_global(VM* vm, int index, Value value);
Value vm_load_global(VM* vm, int index);
void vm_store_local(VM* vm, int index, Value value);
Value vm_load_local(VM* vm, int index);

/* Calls: общий вызов по Value (closure/native) */
Value vm_call_value(VM* vm, Value callee, int arg_count);
Value vm_call_native(VM* vm, NativeFn function, int arg_count);

/* Errors / debug */
void vm_runtime_error(VM* vm, const char* format, ...);
void vm_print_value(Value value);
const char* vm_value_type_name(Value value);
int vm_values_equal(Value a, Value b);
void vm_set_debug(VM* vm, int debug);

/* Object creation */
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

ClassObject* vm_new_class(VM* vm,
                          StringObject* name,
                          int field_count,
                          int method_count);

InstanceObject* vm_new_instance(VM* vm, ClassObject* klass);

/* GC */
void vm_mark_roots(VM* vm);
void vm_collect_garbage(VM* vm);

#endif /* MYTHON_VM_H */