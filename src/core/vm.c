#include "vm.h"
#include "bytecode.h"
#include "memory.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#define INITIAL_STACK_SIZE 256
#define INITIAL_FRAMES 64
#define INITIAL_GLOBALS 256
#define INITIAL_CONSTANTS 256

static uint32_t hash_string(const char* str, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)str[i];
        hash *= 16777619;
    }
    return hash;
}

static int read_i8(uint8_t* ip) {
    return (int8_t)ip[0];
}

static int read_i16(uint8_t* ip) {
    return (int16_t)((ip[0] << 8) | ip[1]);
}

static int32_t read_i32(uint8_t* ip) {
    return (ip[0] << 24) | (ip[1] << 16) | (ip[2] << 8) | ip[3];
}

static float read_f32(uint8_t* ip) {
    uint32_t bits = read_i32(ip);
    union {
        uint32_t i;
        float f;
    } u;
    u.i = bits;
    return u.f;
}

static Value float_to_value(float f) {
    Value val;
    val.type = VAL_FLOAT;
    val.as.floating = f;
    return val;
}

static int value_to_bool(Value value) {
    switch (value.type) {
        case VAL_NIL: return 0;
        case VAL_BOOL: return value.as.boolean;
        case VAL_INT: return value.as.integer != 0;
        case VAL_FLOAT: return value.as.floating != 0.0;
        default: return 1;
    }
}

static int values_equal(Value a, Value b) {
    if (a.type != b.type) {
        if ((a.type == VAL_INT && b.type == VAL_FLOAT) ||
            (a.type == VAL_FLOAT && b.type == VAL_INT)) {
            double da = (a.type == VAL_INT) ? (double)a.as.integer : a.as.floating;
            double db = (b.type == VAL_INT) ? (double)b.as.integer : b.as.floating;
            return fabs(da - db) < 1e-12;
        }
        return 0;
    }

    switch (a.type) {
        case VAL_NIL: return 1;
        case VAL_BOOL: return a.as.boolean == b.as.boolean;
        case VAL_INT: return a.as.integer == b.as.integer;
        case VAL_FLOAT: return fabs(a.as.floating - b.as.floating) < 1e-12;
        case VAL_STRING: {
            StringObject* sa = AS_STRING(a);
            StringObject* sb = AS_STRING(b);
            if (sa->length != sb->length) return 0;
            return memcmp(sa->chars, sb->chars, sa->length) == 0;
        }
        default: return 0;
    }
}

VM* vm_new(void) {
    VM* vm = ALLOCATE(VM, 1);
    vm->stack_capacity = INITIAL_STACK_SIZE;
    vm->stack = ALLOCATE(Value, vm->stack_capacity);
    vm->sp = 0;
    vm->stack_size = 0;

    vm->frame_capacity = INITIAL_FRAMES;
    vm->frames = ALLOCATE(CallFrame, vm->frame_capacity);
    vm->frame_count = 0;

    vm->global_capacity = INITIAL_GLOBALS;
    vm->globals = ALLOCATE(Value, vm->global_capacity);
    vm->global_count = 0;

    vm->constants.values = ALLOCATE(Value, INITIAL_CONSTANTS);
    vm->constants.capacity = INITIAL_CONSTANTS;
    vm->constants.count = 0;

    vm->objects = NULL;
    vm->error_count = 0;
    vm->error_message = NULL;
    vm->line = 0;
    vm->exit_code = 0;

    return vm;
}

void vm_free(VM* vm) {
    FREE_ARRAY(Value, vm->stack, vm->stack_capacity);
    FREE_ARRAY(CallFrame, vm->frames, vm->frame_capacity);
    FREE_ARRAY(Value, vm->globals, vm->global_capacity);
    FREE_ARRAY(Value, vm->constants.values, vm->constants.capacity);

    free_ptr(vm);
}

void vm_push(VM* vm, Value value) {
    if (vm->sp >= vm->stack_capacity) {
        int new_capacity = vm->stack_capacity * 2;
        vm->stack = GROW_ARRAY(Value, vm->stack, vm->stack_capacity, new_capacity);
        vm->stack_capacity = new_capacity;
    }
    vm->stack[vm->sp++] = value;
    if (vm->sp > vm->stack_size) {
        vm->stack_size = vm->sp;
    }
}

Value vm_pop(VM* vm) {
    if (vm->sp == 0) {
        vm_runtime_error(vm, "Stack underflow");
        return NIL_VAL;
    }
    return vm->stack[--vm->sp];
}

Value vm_peek(VM* vm, int distance) {
    if (distance < 0 || distance >= vm->sp) {
        return NIL_VAL;
    }
    return vm->stack[vm->sp - 1 - distance];
}

void vm_push_frame(VM* vm, Bytecode* bytecode, int slot_count) {
    if (vm->frame_count >= vm->frame_capacity) {
        int new_capacity = vm->frame_capacity * 2;
        vm->frames = GROW_ARRAY(CallFrame, vm->frames, vm->frame_capacity, new_capacity);
        vm->frame_capacity = new_capacity;
    }

    CallFrame* frame = &vm->frames[vm->frame_count++];
    frame->bytecode = bytecode;
    frame->ip = bytecode->code;
    frame->slots = ALLOCATE(Value, slot_count);
    frame->slot_count = slot_count;

    for (int i = 0; i < slot_count; i++) {
        frame->slots[i] = NIL_VAL;
    }
}

void vm_pop_frame(VM* vm) {
    if (vm->frame_count == 0) {
        vm_runtime_error(vm, "No frame to pop");
        return;
    }

    CallFrame* frame = &vm->frames[--vm->frame_count];
    FREE_ARRAY(Value, frame->slots, frame->slot_count);
}

void vm_store_global(VM* vm, int index, Value value) {
    if (index < 0) {
        vm_runtime_error(vm, "Invalid global index: %d", index);
        return;
    }

    if (index >= vm->global_capacity) {
        int new_capacity = vm->global_capacity * 2;
        while (new_capacity <= index) new_capacity *= 2;
        vm->globals = GROW_ARRAY(Value, vm->globals, vm->global_capacity, new_capacity);
        vm->global_capacity = new_capacity;
    }

    if (index >= vm->global_count) {
        vm->global_count = index + 1;
    }

    vm->globals[index] = value;
}

Value vm_load_global(VM* vm, int index) {
    if (index < 0 || index >= vm->global_count) {
        vm_runtime_error(vm, "Undefined global: %d", index);
        return NIL_VAL;
    }
    return vm->globals[index];
}

void vm_store_local(VM* vm, int index, Value value) {
    if (vm->frame_count == 0) {
        vm_runtime_error(vm, "No active frame");
        return;
    }

    CallFrame* frame = &vm->frames[vm->frame_count - 1];
    if (index < 0 || index >= frame->slot_count) {
        vm_runtime_error(vm, "Invalid local index: %d", index);
        return;
    }

    frame->slots[index] = value;
}

Value vm_load_local(VM* vm, int index) {
    if (vm->frame_count == 0) {
        vm_runtime_error(vm, "No active frame");
        return NIL_VAL;
    }

    CallFrame* frame = &vm->frames[vm->frame_count - 1];
    if (index < 0 || index >= frame->slot_count) {
        vm_runtime_error(vm, "Invalid local index: %d", index);
        return NIL_VAL;
    }

    return frame->slots[index];
}

static void print_value(Value value) {
    switch (value.type) {
        case VAL_NIL:
            printf("nil");
            break;
        case VAL_BOOL:
            printf(value.as.boolean ? "true" : "false");
            break;
        case VAL_INT:
            printf("%lld", (long long)value.as.integer);
            break;
        case VAL_FLOAT:
            printf("%g", value.as.floating);
            break;
        case VAL_STRING: {
            StringObject* str = AS_STRING(value);
            fwrite(str->chars, 1, str->length, stdout);
            break;
        }
        case VAL_ARRAY:
            printf("[array]");
            break;
        case VAL_FUNCTION:
            printf("[function]");
            break;
        case VAL_NATIVE_FN:
            printf("[native]");
            break;
        case VAL_CLOSURE:
            printf("[closure]");
            break;
        default:
            printf("[unknown]");
            break;
    }
}

StringObject* vm_take_string(VM* vm, char* chars, int length) {
    StringObject* str = ALLOCATE(StringObject, 1);
    str->obj.type = VAL_STRING;
    str->obj.marked = 0;
    str->obj.next = vm->objects;
    vm->objects = (Object*)str;

    str->chars = chars;
    str->length = length;
    str->hash = hash_string(chars, length);

    return str;
}

StringObject* vm_copy_string(VM* vm, const char* chars, int length) {
    char* heap_chars = ALLOCATE(char, length + 1);
    memcpy(heap_chars, chars, length);
    heap_chars[length] = '\0';
    return vm_take_string(vm, heap_chars, length);
}

ArrayObject* vm_new_array(VM* vm, int capacity) {
    ArrayObject* array = ALLOCATE(ArrayObject, 1);
    array->obj.type = VAL_ARRAY;
    array->obj.marked = 0;
    array->obj.next = vm->objects;
    vm->objects = (Object*)array;

    if (capacity > 0) {
        array->items = ALLOCATE(Value, capacity);
        array->capacity = capacity;
    } else {
        array->items = NULL;
        array->capacity = 0;
    }

    array->count = 0;
    return array;
}

void vm_array_append(VM* vm, ArrayObject* array, Value value) {
    (void)vm;
    if (array->count >= array->capacity) {
        int new_capacity = array->capacity == 0 ? 4 : array->capacity * 2;
        array->items = GROW_ARRAY(Value, array->items, array->capacity, new_capacity);
        array->capacity = new_capacity;
    }
    array->items[array->count++] = value;
}

int vm_values_equal(Value a, Value b) {
    return values_equal(a, b);
}

void vm_print_value(Value value) {
    print_value(value);
}

const char* vm_value_type_name(Value value) {
    switch (value.type) {
        case VAL_NIL: return "nil";
        case VAL_BOOL: return "bool";
        case VAL_INT: return "int";
        case VAL_FLOAT: return "float";
        case VAL_STRING: return "string";
        case VAL_ARRAY: return "array";
        case VAL_FUNCTION: return "function";
        case VAL_NATIVE_FN: return "native";
        case VAL_CLOSURE: return "closure";
        default: return "unknown";
    }
}

void vm_runtime_error(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);

    fprintf(stderr, "Runtime error: ");
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

    va_end(args);
    vm->error_count++;
}

Value vm_call_function(VM* vm, int function_index, int arg_count) {
    if (function_index < 0 || function_index >= vm->constants.count) {
        vm_runtime_error(vm, "Invalid function index: %d", function_index);
        return NIL_VAL;
    }

    Value func_val = vm->constants.values[function_index];
    if (!IS_FUNCTION(func_val)) {
        vm_runtime_error(vm, "Not a function at index: %d", function_index);
        return NIL_VAL;
    }

    FunctionObject* func = AS_FUNCTION(func_val);
    vm_push_frame(vm, func->bytecode, func->arity);

    CallFrame* frame = &vm->frames[vm->frame_count - 1];

    for (int i = 0; i < arg_count && i < func->arity; i++) {
        frame->slots[i] = vm_pop(vm);
    }

    InterpretResult result = vm_run(vm, func->bytecode);
    if (result != INTERPRET_OK) {
        return NIL_VAL;
    }

    if (vm->sp > 0) {
        return vm_pop(vm);
    }
    return NIL_VAL;
}

Value vm_call_native(VM* vm, NativeFn function, int arg_count) {
    (void)vm;
    Value* args = ALLOCATE(Value, arg_count);
    for (int i = 0; i < arg_count; i++) {
        args[arg_count - 1 - i] = vm_pop(vm);
    }

    Value result = function(arg_count, args);
    FREE_ARRAY(Value, args, arg_count);
    return result;
}

static InterpretResult run(VM* vm) {
    if (vm->frame_count == 0) {
        return INTERPRET_OK;
    }

    CallFrame* frame = &vm->frames[vm->frame_count - 1];
    uint8_t* ip = frame->ip;
    // Bytecode* bc = frame->bytecode;

    while (1) {
        uint8_t instruction = *ip++;

        switch (instruction) {
            case OP_NOP:
                break;

            case OP_HALT:
                return INTERPRET_OK;

            case OP_ADD: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, INT_VAL(AS_INT(a) + AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, FLOAT_VAL(da + db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_SUB: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, INT_VAL(AS_INT(a) - AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, FLOAT_VAL(da - db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_MUL: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, INT_VAL(AS_INT(a) * AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, FLOAT_VAL(da * db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_DIV: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(b) && AS_INT(b) == 0) {
                    vm_runtime_error(vm, "Division by zero");
                    return INTERPRET_RUNTIME_ERROR;
                }
                if (IS_FLOAT(b) && fabs(AS_FLOAT(b)) < 1e-12) {
                    vm_runtime_error(vm, "Division by zero");
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, INT_VAL(AS_INT(a) / AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, FLOAT_VAL(da / db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_MOD: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    if (AS_INT(b) == 0) {
                        vm_runtime_error(vm, "Modulo by zero");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    vm_push(vm, INT_VAL(AS_INT(a) % AS_INT(b)));
                } else {
                    vm_runtime_error(vm, "Operands must be integers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_NEG: {
                Value a = vm_pop(vm);

                if (IS_INT(a)) {
                    vm_push(vm, INT_VAL(-AS_INT(a)));
                } else if (IS_FLOAT(a)) {
                    vm_push(vm, FLOAT_VAL(-AS_FLOAT(a)));
                } else {
                    vm_runtime_error(vm, "Operand must be a number");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_NOT: {
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(!value_to_bool(a)));
                break;
            }

            case OP_EQ: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(values_equal(a, b)));
                break;
            }

            case OP_NEQ: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(!values_equal(a, b)));
                break;
            }

            case OP_LT: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, BOOL_VAL(AS_INT(a) < AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da < db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_LE: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, BOOL_VAL(AS_INT(a) <= AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da <= db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_GT: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, BOOL_VAL(AS_INT(a) > AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da > db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_GE: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);

                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, BOOL_VAL(AS_INT(a) >= AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da >= db));
                } else {
                    vm_runtime_error(vm, "Operands must be numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_AND: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(value_to_bool(a) && value_to_bool(b)));
                break;
            }

            case OP_OR: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(value_to_bool(a) || value_to_bool(b)));
                break;
            }

            case OP_PUSH_I8: {
                int8_t val = read_i8(ip);
                ip += 1;
                vm_push(vm, INT_VAL(val));
                break;
            }

            case OP_PUSH_NIL:
                ip += 1;
                vm_push(vm, NIL_VAL);
                break;

            case OP_PUSH_TRUE:
                ip += 1;
                vm_push(vm, BOOL_VAL(1));
                break;

            case OP_PUSH_FALSE:
                ip += 1;
                vm_push(vm, BOOL_VAL(0));
                break;

            case OP_STORE_LOCAL_8: {
                int index = read_i8(ip);
                ip += 1;
                Value val = vm_peek(vm, 0);
                vm_store_local(vm, index, val);
                break;
            }

            case OP_LOAD_LOCAL_8: {
                int index = read_i8(ip);
                ip += 1;
                Value val = vm_load_local(vm, index);
                vm_push(vm, val);
                break;
            }

            case OP_JUMP_8: {
                int offset = read_i8(ip);
                ip += 1;
                ip += offset;
                break;
            }

            case OP_JUMP_IF_TRUE_8: {
                int offset = read_i8(ip);
                ip += 1;
                Value condition = vm_pop(vm);
                if (value_to_bool(condition)) {
                    ip += offset;
                }
                break;
            }

            case OP_JUMP_IF_FALSE_8: {
                int offset = read_i8(ip);
                ip += 1;
                Value condition = vm_pop(vm);
                if (!value_to_bool(condition)) {
                    ip += offset;
                }
                break;
            }

            case OP_POP:
                ip += 1;
                vm_pop(vm);
                break;

            case OP_PUSH_I16: {
                int16_t val = read_i16(ip);
                ip += 2;
                vm_push(vm, INT_VAL(val));
                break;
            }

            case OP_STORE_LOCAL_16: {
                int index = read_i16(ip);
                ip += 2;
                Value val = vm_peek(vm, 0);
                vm_store_local(vm, index, val);
                break;
            }

            case OP_LOAD_LOCAL_16: {
                int index = read_i16(ip);
                ip += 2;
                Value val = vm_load_local(vm, index);
                vm_push(vm, val);
                break;
            }

            case OP_JUMP_16: {
                int16_t offset = read_i16(ip);
                ip += 2;
                ip += offset;
                break;
            }

            case OP_JUMP_IF_TRUE_16: {
                int16_t offset = read_i16(ip);
                ip += 2;
                Value condition = vm_pop(vm);
                if (value_to_bool(condition)) {
                    ip += offset;
                }
                break;
            }

            case OP_JUMP_IF_FALSE_16: {
                int16_t offset = read_i16(ip);
                ip += 2;
                Value condition = vm_pop(vm);
                if (!value_to_bool(condition)) {
                    ip += offset;
                }
                break;
            }

            case OP_CALL_8: {
                int arg_count = read_i8(ip);
                ip += 1;
                Value result = vm_call_function(vm, arg_count, arg_count);
                vm_push(vm, result);
                break;
            }

            case OP_PUSH_I32: {
                int32_t val = read_i32(ip);
                ip += 4;
                vm_push(vm, INT_VAL(val));
                break;
            }

            case OP_PUSH_F32: {
                float val = read_f32(ip);
                ip += 4;
                vm_push(vm, float_to_value(val));
                break;
            }

            case OP_JUMP_32: {
                int32_t offset = read_i32(ip);
                ip += 4;
                ip += offset;
                break;
            }

            case OP_CALL_16: {
                int arg_count = read_i16(ip);
                ip += 2;
                Value result = vm_call_function(vm, arg_count, arg_count);
                vm_push(vm, result);
                break;
            }

            case OP_LOAD_CONST: {
                int const_index = read_i32(ip);
                ip += 4;
                if (const_index < 0 || const_index >= vm->constants.count) {
                    vm_runtime_error(vm, "Invalid constant index: %d", const_index);
                    return INTERPRET_RUNTIME_ERROR;
                }
                vm_push(vm, vm->constants.values[const_index]);
                break;
            }

            case OP_ARRAY_NEW: {
                int size = AS_INT(vm_pop(vm));
                ArrayObject* array = vm_new_array(vm, size);
                vm_push(vm, OBJECT_VAL(array));
                break;
            }

            case OP_ARRAY_GET: {
                int index = AS_INT(vm_pop(vm));
                Value array_val = vm_pop(vm);

                if (!IS_ARRAY(array_val)) {
                    vm_runtime_error(vm, "Expected array");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ArrayObject* array = AS_ARRAY(array_val);
                if (index < 0 || index >= array->count) {
                    vm_runtime_error(vm, "Array index out of bounds: %d", index);
                    return INTERPRET_RUNTIME_ERROR;
                }

                vm_push(vm, array->items[index]);
                break;
            }

            case OP_ARRAY_SET: {
                Value value = vm_pop(vm);
                int index = AS_INT(vm_pop(vm));
                Value array_val = vm_pop(vm);

                if (!IS_ARRAY(array_val)) {
                    vm_runtime_error(vm, "Expected array");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ArrayObject* array = AS_ARRAY(array_val);
                if (index < 0) {
                    vm_runtime_error(vm, "Negative array index: %d", index);
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (index >= array->capacity) {
                    int new_capacity = array->capacity;
                    while (new_capacity <= index) new_capacity *= 2;
                    array->items = GROW_ARRAY(Value, array->items, array->capacity, new_capacity);
                    array->capacity = new_capacity;
                }

                array->items[index] = value;
                if (index >= array->count) {
                    array->count = index + 1;
                }
                break;
            }

            case OP_ARRAY_LEN: {
                Value array_val = vm_pop(vm);
                if (!IS_ARRAY(array_val)) {
                    vm_runtime_error(vm, "Expected array");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ArrayObject* array = AS_ARRAY(array_val);
                vm_push(vm, INT_VAL(array->count));
                break;
            }

            case OP_CALL_NATIVE: {
                NativeFn native = (NativeFn)vm_pop(vm).as.integer;
                int arg_count = AS_INT(vm_pop(vm));
                Value result = vm_call_native(vm, native, arg_count);
                vm_push(vm, result);
                break;
            }

            case OP_RETURN: {
                Value result = vm_pop(vm);
                vm_pop_frame(vm);
                if (vm->frame_count == 0) {
                    vm_push(vm, result);
                    return INTERPRET_OK;
                }
                vm_push(vm, result);
                frame = &vm->frames[vm->frame_count - 1];
                ip = frame->ip;
                break;
            }

            case OP_RETURN_NIL:
                vm_pop_frame(vm);
                if (vm->frame_count == 0) {
                    return INTERPRET_OK;
                }
                frame = &vm->frames[vm->frame_count - 1];
                ip = frame->ip;
                vm_push(vm, NIL_VAL);
                break;

            case OP_PRINT: {
                Value value = vm_pop(vm);
                print_value(value);
                printf("\n");
                break;
            }

            case OP_DUP: {
                Value top = vm_peek(vm, 0);
                vm_push(vm, top);
                break;
            }

            case OP_BREAK:
                return INTERPRET_OK;

            default:
                vm_runtime_error(vm, "Unknown opcode: %d", instruction);
                return INTERPRET_RUNTIME_ERROR;
        }

        frame->ip = ip;
    }
}

InterpretResult vm_run(VM* vm, Bytecode* bytecode) {
    vm_push_frame(vm, bytecode, 0);
    return run(vm);
}

InterpretResult vm_interpret(VM* vm, const char* source) {
    (void)vm;
    (void)source;
    // Bytecode* bc = bytecode_new();

    return INTERPRET_COMPILE_ERROR;
}