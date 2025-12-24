#include "vm.h"
#include "bytecode.h"
#include "utils/memory.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#define INITIAL_STACK_SIZE 256
#define INITIAL_FRAMES     64
#define INITIAL_GLOBALS    256
#define INITIAL_CONSTANTS  256

static const char* opcode_name(uint8_t opcode) {
    switch ((OpCode)opcode) {
        case OP_NOP:                return "NOP";
        case OP_HALT:               return "HALT";
        case OP_ADD:                return "ADD";
        case OP_SUB:                return "SUB";
        case OP_MUL:                return "MUL";
        case OP_DIV:                return "DIV";
        case OP_MOD:                return "MOD";
        case OP_NEG:                return "NEG";
        case OP_NOT:                return "NOT";
        case OP_EQ:                 return "EQ";
        case OP_NEQ:                return "NEQ";
        case OP_LT:                 return "LT";
        case OP_LE:                 return "LE";
        case OP_GT:                 return "GT";
        case OP_GE:                 return "GE";
        case OP_AND:                return "AND";
        case OP_OR:                 return "OR";
        case OP_LOAD_CONST_U16:     return "LOAD_CONST";
        case OP_LOAD_LOCAL_U8:      return "LOAD_LOCAL";
        case OP_STORE_LOCAL_U8:     return "STORE_LOCAL";
        case OP_LOAD_UPVALUE_U8:    return "LOAD_UPVALUE";
        case OP_STORE_UPVALUE_U8:   return "STORE_UPVALUE";
        case OP_JUMP_U16:           return "JUMP";
        case OP_JUMP_IF_FALSE_U16:  return "JUMP_IF_FALSE";
        case OP_JUMP_IF_TRUE_U16:   return "JUMP_IF_TRUE";
        case OP_CALL_U8:            return "CALL_U8";
        case OP_CALL_U16:           return "CALL_U16";
        case OP_RETURN:             return "RETURN";
        case OP_RETURN_NIL:         return "RETURN_NIL";
        case OP_NEW_CLOSURE:        return "NEW_CLOSURE";
        case OP_NEW_CLASS:          return "NEW_CLASS";
        case OP_LOAD_FIELD_U8:      return "LOAD_FIELD";
        case OP_STORE_FIELD_U8:     return "STORE_FIELD";
        case OP_CALL_METHOD_U8:     return "CALL_METHOD";
        case OP_ARRAY_NEW_U8:       return "ARRAY_NEW";
        case OP_ARRAY_GET:          return "ARRAY_GET";
        case OP_ARRAY_SET:          return "ARRAY_SET";
        case OP_ARRAY_LEN:          return "ARRAY_LEN";
        case OP_LOAD_GLOBAL_U16:    return "LOAD_GLOBAL";
        case OP_STORE_GLOBAL_U16:   return "STORE_GLOBAL";
        case OP_PRINT:              return "PRINT";
        case OP_BREAK:              return "BREAK";
        default:                    return "UNKNOWN";
    }
}

void vm_set_debug(VM* vm, int debug) {
    vm->debug = debug;
}

static void debug_print_value(Value v) {
    vm_print_value(v);
}

static void debug_print_instruction(Bytecode* bc, uint8_t* ip) {
    size_t offset = (size_t)(ip - bc->code);
    uint8_t opcode = *ip;
    OpCode op = (OpCode)opcode;

    fprintf(stderr, "%04zu  %s", offset, opcode_name(opcode));

    switch (op) {
        case OP_LOAD_CONST_U16: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            fprintf(stderr, " %u", idx);
            if (idx < bc->const_count) {
                Constant* c = &bc->constants[idx];
                fprintf(stderr, " (");
                switch (c->type) {
                    case CONST_INT:    fprintf(stderr, "INT %lld", (long long)c->int_val); break;
                    case CONST_FLOAT:  fprintf(stderr, "FLOAT %g", c->float_val); break;
                    case CONST_STRING: fprintf(stderr, "STRING \"%s\"", c->str_val); break;
                    default:           fprintf(stderr, "?"); break;
                }
                fprintf(stderr, ")");
            }
            break;
        }

        case OP_LOAD_LOCAL_U8:
        case OP_STORE_LOCAL_U8:
        case OP_LOAD_UPVALUE_U8:
        case OP_STORE_UPVALUE_U8:
        case OP_LOAD_FIELD_U8:
        case OP_STORE_FIELD_U8:
        case OP_CALL_METHOD_U8:
        case OP_ARRAY_NEW_U8: {
            uint8_t b = ip[1];
            fprintf(stderr, " %u", b);
            break;
        }

        case OP_JUMP_U16:
        case OP_JUMP_IF_FALSE_U16:
        case OP_JUMP_IF_TRUE_U16: {
            int16_t off = (int16_t)(ip[1] << 8 | ip[2]);
            size_t target = offset + 3 + off;
            fprintf(stderr, " %d -> %zu", (int)off, target);
            break;
        }

        case OP_CALL_U8: {
            uint8_t argc = ip[1];
            fprintf(stderr, " argc=%u", argc);
            break;
        }
        case OP_CALL_U16: {
            uint16_t argc = (uint16_t)(ip[1] << 8 | ip[2]);
            fprintf(stderr, " argc=%u", argc);
            break;
        }

        case OP_LOAD_GLOBAL_U16:
        case OP_STORE_GLOBAL_U16: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            fprintf(stderr, " %u", idx);
            break;
        }

        case OP_NEW_CLOSURE:
        case OP_NEW_CLASS: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            fprintf(stderr, " %u", idx);
            break;
        }

        default:
            // без операндов
            break;
    }

    fprintf(stderr, "\n");
}


static void debug_print_stack(VM* vm) {
    fprintf(stderr, "  Stack (size=%d):\n", vm->stack_size);
    for (int i = vm->stack_size - 1; i >= 0; i--) {
        fprintf(stderr, "    [%2d] = ", i);
        debug_print_value(vm->stack[i]);
        fprintf(stderr, "\n");
    }
}

static void debug_print_frames(VM* vm) {
    fprintf(stderr, "  Frames (count=%d):\n", vm->frame_count);
    for (int i = vm->frame_count - 1; i >= 0; i--) {
        CallFrame* f = &vm->frames[i];
        int ip = (int)(f->ip - f->bytecode->code);
        fprintf(stderr,
                "    frame[%d]: func_idx=%d ip=%d slots_offset=%d slot_count=%d",
                i,
                f->closure ? f->closure->function->func_index : -1,
                ip,
                f->slots_offset,
                f->slot_count);
        if (f->closure && f->closure->function && f->closure->function->name) {
            fprintf(stderr, " name=%s", f->closure->function->name);
        }
        fprintf(stderr, "\n");
    }
}

static void debug_print_locals(VM* vm) {
    if (vm->frame_count == 0) return;
    CallFrame* f = &vm->frames[vm->frame_count - 1];
    fprintf(stderr, "  Locals (frame top, count=%d):\n", f->slot_count);
    for (int i = 0; i < f->slot_count; i++) {
        int idx = f->slots_offset + i;
        fprintf(stderr, "    local[%2d] = ", i);
        debug_print_value(vm->stack[idx]);
        fprintf(stderr, "\n");
    }
}

static void debug_print_globals(VM* vm) {
    fprintf(stderr, "  Globals (count=%d):\n", vm->global_count);
    for (int i = 0; i < vm->global_count; i++) {
        fprintf(stderr, "    g[%2d] = ", i);
        debug_print_value(vm->globals[i]);
        fprintf(stderr, "\n");
    }
}

static void debug_print_upvalues(VM* vm) {
    if (vm->frame_count == 0) return;
    CallFrame* f = &vm->frames[vm->frame_count - 1];
    if (!f->closure) return;

    ClosureObject* clo = f->closure;
    fprintf(stderr, "  Upvalues (count=%d):\n", clo->upvalue_count);
    for (int i = 0; i < clo->upvalue_count; i++) {
        Upvalue* uv = clo->upvalues[i];
        fprintf(stderr, "    uv[%2d] = ", i);
        if (!uv) {
            fprintf(stderr, "<null>\n");
        } else if (uv->location) {
            debug_print_value(*uv->location);
            fprintf(stderr, " (open)\n");
        } else {
            debug_print_value(uv->closed);
            fprintf(stderr, " (closed)\n");
        }
    }
}

static uint32_t hash_string(const char* str, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)str[i];
        hash *= 16777619;
    }
    return hash;
}

static int value_to_bool(Value value) {
    switch (value.type) {
        case VAL_NIL:   return 0;
        case VAL_BOOL:  return value.as.boolean;
        case VAL_INT:   return value.as.integer != 0;
        case VAL_FLOAT: return value.as.floating != 0.0;
        default:        return 1;
    }
}

int vm_values_equal(Value a, Value b) {
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
        case VAL_NIL:   return 1;
        case VAL_BOOL:  return a.as.boolean == b.as.boolean;
        case VAL_INT:   return a.as.integer == b.as.integer;
        case VAL_FLOAT: return fabs(a.as.floating - b.as.floating) < 1e-12;
        case VAL_STRING: {
            StringObject* sa = AS_STRING(a);
            StringObject* sb = AS_STRING(b);
            if (sa->length != sb->length) return 0;
            return memcmp(sa->chars, sb->chars, sa->length) == 0;
        }
        default:
            return 0;
    }
}

const char* vm_value_type_name(Value v) {
    switch (v.type) {
        case VAL_NIL:      return "nil";
        case VAL_BOOL:     return "bool";
        case VAL_INT:      return "int";
        case VAL_FLOAT:    return "float";
        case VAL_STRING:   return "string";
        case VAL_ARRAY:    return "array";
        case VAL_FUNCTION: return "function";
        case VAL_NATIVE_FN:return "native";
        case VAL_CLOSURE:  return "closure";
        case VAL_CLASS:    return "class";
        case VAL_INSTANCE: return "instance";
        default:           return "unknown";
    }
}

void vm_print_value(Value v) {
    switch (v.type) {
        case VAL_NIL:   printf("nil"); break;
        case VAL_BOOL:  printf("%s", v.as.boolean ? "true" : "false"); break;
        case VAL_INT:   printf("%lld", (long long)v.as.integer); break;
        case VAL_FLOAT: printf("%g", v.as.floating); break;
        case VAL_STRING: {
            StringObject* s = AS_STRING(v);
            printf("%.*s", s->length, s->chars);
            break;
        }
        case VAL_ARRAY:    printf("[array]"); break;
        case VAL_FUNCTION: printf("[function]"); break;
        case VAL_NATIVE_FN:printf("[native]"); break;
        case VAL_CLOSURE:  printf("[closure]"); break;
        case VAL_CLASS:    printf("[class]"); break;
        case VAL_INSTANCE: printf("[instance]"); break;
        default:           printf("[unknown]"); break;
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
    vm->bytes_allocated = 0;
    vm->next_gc = 1024 * 1024;

    vm->error_count = 0;
    vm->error_message = NULL;
    vm->line = 0;
    vm->debug = 0;
    vm->debug_level = DEBUG_NONE;
    vm->exit_code = 0;

    return vm;
}

void vm_free(VM* vm) {
    if (!vm) return;

    FREE_ARRAY(Value, vm->stack, vm->stack_capacity);
    FREE_ARRAY(CallFrame, vm->frames, vm->frame_capacity);
    FREE_ARRAY(Value, vm->globals, vm->global_capacity);
    FREE_ARRAY(Value, vm->constants.values, vm->constants.capacity);

    // TODO: пройтись по списку vm->objects и освободить GC-объекты,
    // когда будет реализован GC.

    free_ptr(vm);
}

void vm_push(VM* vm, Value value) {
    if (vm->sp >= vm->stack_capacity) {
        int old = vm->stack_capacity;
        int new_cap = old * 2;
        vm->stack = GROW_ARRAY(Value, vm->stack, old, new_cap);
        vm->stack_capacity = new_cap;
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
    vm_push_frame_with_ip(vm, bytecode, slot_count, bytecode->code);
}

void vm_push_frame_with_ip(VM* vm, Bytecode* bytecode, int slot_count, uint8_t* ip_start) {
    if (vm->frame_count >= vm->frame_capacity) {
        int old = vm->frame_capacity;
        int new_cap = old * 2;
        vm->frames = GROW_ARRAY(CallFrame, vm->frames, old, new_cap);
        vm->frame_capacity = new_cap;
    }

    CallFrame* frame = &vm->frames[vm->frame_count++];
    frame->bytecode = bytecode;
    frame->ip = ip_start;

    frame->slots_offset = vm->sp;
    frame->slot_count = slot_count;
    frame->closure = NULL;

    for (int i = 0; i < slot_count; i++) {
        vm_push(vm, NIL_VAL);
    }
}

void vm_pop_frame(VM* vm) {
    if (vm->frame_count == 0) return;
    CallFrame* frame = &vm->frames[vm->frame_count - 1];

    vm->sp = frame->slots_offset;
    vm->frame_count--;
}

void vm_store_global(VM* vm, int index, Value value) {
    if (index < 0) return;

    if (index >= vm->global_capacity) {
        int old = vm->global_capacity;
        (void)old;
        int new_cap = index + 1;
        vm->globals = GROW_ARRAY(Value, vm->globals, old, new_cap);
        vm->global_capacity = new_cap;
    }

    vm->globals[index] = value;
    if (index >= vm->global_count) {
        vm->global_count = index + 1;
    }
}


Value vm_load_global(VM* vm, int index) {
    if (index < 0 || index >= vm->global_count) {
        return NIL_VAL;
    }
    return vm->globals[index];
}

void vm_store_local(VM* vm, int index, Value value) {
    if (vm->frame_count == 0) return;
    CallFrame* frame = &vm->frames[vm->frame_count - 1];
    if (index < 0 || index >= frame->slot_count) return;
    vm->stack[frame->slots_offset + index] = value;
}

Value vm_load_local(VM* vm, int index) {
    if (vm->frame_count == 0) return NIL_VAL;
    CallFrame* frame = &vm->frames[vm->frame_count - 1];
    if (index < 0 || index >= frame->slot_count) return NIL_VAL;
    return vm->stack[frame->slots_offset + index];
}

void vm_runtime_error(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    fprintf(stderr, "Runtime error: ");
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    va_end(args);

    vm->error_count++;
    vm->exit_code = 1;
}

static void link_object(VM* vm, Object* obj, uint8_t type) {
    obj->type = type;
    obj->marked = 0;
    obj->next = vm->objects;
    vm->objects = obj;
}

StringObject* vm_take_string(VM* vm, char* chars, int length) {
    StringObject* str = ALLOCATE(StringObject, 1);
    link_object(vm, (Object*)str, VAL_STRING);
    str->chars = chars;
    str->length = length;
    str->hash = hash_string(chars, length);
    return str;
}

StringObject* vm_copy_string(VM* vm, const char* chars, int length) {
    char* copy = (char*)allocate((size_t)length + 1);
    memcpy(copy, chars, length);
    copy[length] = '\0';
    return vm_take_string(vm, copy, length);
}

ArrayObject* vm_new_array(VM* vm, int capacity) {
    ArrayObject* arr = ALLOCATE(ArrayObject, 1);
    link_object(vm, (Object*)arr, VAL_ARRAY);
    arr->capacity = capacity > 0 ? capacity : 0;
    arr->count = 0;
    arr->items = arr->capacity > 0 ? ALLOCATE(Value, arr->capacity) : NULL;
    return arr;
}

void vm_array_append(VM* vm, ArrayObject* array, Value value) {
    (void)vm;
    if (array->count >= array->capacity) {
        int old = array->capacity;
        int new_cap = old < 8 ? 8 : old * 2;
        array->items = GROW_ARRAY(Value, array->items, old, new_cap);
        array->capacity = new_cap;
    }
    array->items[array->count++] = value;
}

NativeFunctionObject* vm_new_native_function(VM* vm,
                                             const char* name,
                                             NativeFn function,
                                             int arity) {
    NativeFunctionObject* nf = ALLOCATE(NativeFunctionObject, 1);
    link_object(vm, (Object*)nf, VAL_NATIVE_FN);
    nf->function = function;
    int len = (int)strlen(name);
    nf->name = (char*)allocate((size_t)len + 1);
    memcpy(nf->name, name, (size_t)len + 1);
    nf->arity = arity;
    return nf;
}

FunctionObject* vm_new_function(VM* vm,
                                const char* name,
                                int arity,
                                int local_count,
                                int upvalue_count,
                                Bytecode* bytecode,
                                int func_index) {
    FunctionObject* fn = ALLOCATE(FunctionObject, 1);
    link_object(vm, (Object*)fn, VAL_FUNCTION);
    int len = (int)strlen(name);
    fn->name = (char*)allocate((size_t)len + 1);
    memcpy(fn->name, name, (size_t)len + 1);
    fn->arity = arity;
    fn->local_count = local_count;
    fn->upvalue_count = upvalue_count;
    fn->bytecode = bytecode;
    fn->func_index = func_index;
    return fn;
}

ClosureObject* vm_new_closure(VM* vm, FunctionObject* function) {
    ClosureObject* cl = ALLOCATE(ClosureObject, 1);
    link_object(vm, (Object*)cl, VAL_CLOSURE);
    cl->function = function;
    cl->upvalue_count = function->upvalue_count;
    if (cl->upvalue_count > 0) {
        cl->upvalues = ALLOCATE(Upvalue*, cl->upvalue_count);
        for (int i = 0; i < cl->upvalue_count; i++) {
            cl->upvalues[i] = NULL;
        }
    } else {
        cl->upvalues = NULL;
    }
    return cl;
}

ClassObject* vm_new_class(VM* vm,
                          StringObject* name,
                          int field_count,
                          int method_count) {
    ClassObject* klass = ALLOCATE(ClassObject, 1);
    link_object(vm, (Object*)klass, VAL_CLASS);
    klass->name = name;

    klass->field_count = field_count;
    klass->method_count = method_count;

    if (field_count > 0) {
        klass->field_names = ALLOCATE(StringObject*, field_count);
        for (int i = 0; i < field_count; i++) {
            klass->field_names[i] = NULL;
        }
    } else {
        klass->field_names = NULL;
    }

    if (method_count > 0) {
        klass->methods = ALLOCATE(ClosureObject*, method_count);
        for (int i = 0; i < method_count; i++) {
            klass->methods[i] = NULL;
        }
    } else {
        klass->methods = NULL;
    }

    return klass;
}

InstanceObject* vm_new_instance(VM* vm, ClassObject* klass) {
    InstanceObject* inst = ALLOCATE(InstanceObject, 1);
    link_object(vm, (Object*)inst, VAL_INSTANCE);
    inst->klass = klass;
    inst->field_count = klass->field_count;
    if (inst->field_count > 0) {
        inst->fields = ALLOCATE(Value, inst->field_count);
        for (int i = 0; i < inst->field_count; i++) {
            inst->fields[i] = NIL_VAL;
        }
    } else {
        inst->fields = NULL;
    }
    return inst;
}

Value vm_call_native(VM* vm, NativeFn function, int arg_count) {
    Value result = function(arg_count, &vm->stack[vm->sp - arg_count]);
    vm->sp -= arg_count;
    vm_push(vm, result);
    return result;
}

Value vm_call_value(VM* vm, Value callee, int arg_count) {
    if (IS_NATIVE(callee)) {
        NativeFunctionObject* nf = AS_NATIVE(callee);
        return vm_call_native(vm, nf->function, arg_count);
    }
    if (IS_CLOSURE(callee)) {
        ClosureObject* cl = AS_CLOSURE(callee);
        FunctionObject* fn = cl->function;
        if (fn->bytecode == NULL) {
            vm_runtime_error(vm, "Closure has no bytecode");
            return NIL_VAL;
        }

        vm_push_frame_with_ip(vm,
                              fn->bytecode,
                              fn->local_count,
                              fn->bytecode->code + fn->bytecode->functions[fn->func_index].code_start);

        CallFrame* new_frame = &vm->frames[vm->frame_count - 1];
        new_frame->closure = cl;

        for (int i = arg_count - 1; i >= 0; i--) {
            Value v = vm_pop(vm);
            vm_store_local(vm, i, v);
        }

        return NIL_VAL;
    }

    vm_runtime_error(vm, "Attempt to call non-callable value");
    return NIL_VAL;
}

InterpretResult vm_interpret(VM* vm, const char* source) {
    (void)vm;
    (void)source;
    // Компилятор ещё не подключен; тут будет compile+vm_run.
    return INTERPRET_COMPILE_ERROR;
}

static uint16_t read_u16(uint8_t* ip) {
    return (uint16_t)((ip[0] << 8) | ip[1]);
}

static void vm_load_constants(VM* vm, Bytecode* bc) {
    if (vm->constants.capacity < (int)bc->const_count) {
        int old = vm->constants.capacity;
        (void)old;
        int new_cap = (int)bc->const_count;
        vm->constants.values = GROW_ARRAY(Value, vm->constants.values, old, new_cap);
        vm->constants.capacity = new_cap;
    }

    vm->constants.count = (int)bc->const_count;

    for (size_t i = 0; i < bc->const_count; i++) {
        Constant* c = &bc->constants[i];
        Value v = NIL_VAL;

        switch (c->type) {
            case CONST_INT:
                v = INT_VAL(c->int_val);
                break;
            case CONST_FLOAT:
                v = FLOAT_VAL(c->float_val);
                break;
            case CONST_STRING: {
                int len = (int)strlen(c->str_val);
                StringObject* s = vm_copy_string(vm, c->str_val, len);
                v = OBJECT_VAL(s);
                break;
            }
            case CONST_CLOSURE: {
                // пока как "функция с индексом"
                v.type = VAL_FUNCTION;
                v.as.integer = (int)c->closure.func_idx;
                break;
            }
            default:
                v = NIL_VAL;
                break;
        }

        vm->constants.values[i] = v;
    }
}


InterpretResult vm_run(VM* vm, Bytecode* bytecode) {
    if (!bytecode) return INTERPRET_RUNTIME_ERROR;
    vm_load_constants(vm, bytecode);

    vm->frame_count = 0;
    vm->sp = 0;

    if (bytecode->func_count == 0) {
        vm_runtime_error(vm, "No functions in bytecode");
        return INTERPRET_RUNTIME_ERROR;
    }

    Function* main_meta = &bytecode->functions[0];

    FunctionObject* main_fn = vm_new_function(
        vm,
        main_meta->name ? main_meta->name : "<fn>",
        (int)main_meta->arity,
        main_meta->max_locals,
        main_meta->n_upvalues,
        bytecode,
        0
    );
    ClosureObject* main_clo = vm_new_closure(vm, main_fn);

    vm_push_frame_with_ip(
        vm,
        bytecode,
        main_fn->local_count,
        bytecode->code + main_meta->code_start
    );
    vm->frames[vm->frame_count - 1].closure = main_clo;

    for (;;) {
        if (vm->frame_count == 0) break;
        CallFrame* frame = &vm->frames[vm->frame_count - 1];
        uint8_t* ip = frame->ip;
        uint8_t opcode = *ip++;
        frame->ip = ip;

        if (vm->debug_level >= DEBUG_OPS) {
            debug_print_instruction(frame->bytecode, ip - 1);
        }
        if (vm->debug_level >= DEBUG_STACK) {
            debug_print_stack(vm);
            debug_print_frames(vm);
            debug_print_locals(vm);
            debug_print_upvalues(vm);
        }
        if (vm->debug_level >= DEBUG_GLOBAL) {
            debug_print_globals(vm);
        }

        switch ((OpCode)opcode) {
            case OP_ADD: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, INT_VAL(AS_INT(a) + AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) &&
                           (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, FLOAT_VAL(da + db));
                } else {
                    vm_runtime_error(vm, "ADD on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUB: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, INT_VAL(AS_INT(a) - AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) &&
                           (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, FLOAT_VAL(da - db));
                } else {
                    vm_runtime_error(vm, "SUB on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_MUL: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if (IS_INT(a) && IS_INT(b)) {
                    vm_push(vm, INT_VAL(AS_INT(a) * AS_INT(b)));
                } else if ((IS_INT(a) || IS_FLOAT(a)) &&
                           (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, FLOAT_VAL(da * db));
                } else {
                    vm_runtime_error(vm, "MUL on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_DIV: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if ((IS_INT(a) || IS_FLOAT(a)) &&
                    (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    if (db == 0.0) {
                        vm_runtime_error(vm, "Division by zero");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    vm_push(vm, FLOAT_VAL(da / db));
                } else {
                    vm_runtime_error(vm, "DIV on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_MOD: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if (IS_INT(a) && IS_INT(b)) {
                    int64_t bb = AS_INT(b);
                    if (bb == 0) {
                        vm_runtime_error(vm, "Modulo by zero");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    vm_push(vm, INT_VAL(AS_INT(a) % bb));
                } else {
                    vm_runtime_error(vm, "MOD on non-integers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_NEG: {
                Value v = vm_pop(vm);
                if (IS_INT(v)) {
                    vm_push(vm, INT_VAL(-AS_INT(v)));
                } else if (IS_FLOAT(v)) {
                    vm_push(vm, FLOAT_VAL(-AS_FLOAT(v)));
                } else {
                    vm_runtime_error(vm, "NEG on non-number");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_NOT: {
                Value v = vm_pop(vm);
                vm_push(vm, BOOL_VAL(!value_to_bool(v)));
                break;
            }
            case OP_EQ: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(vm_values_equal(a, b)));
                break;
            }
            case OP_NEQ: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(!vm_values_equal(a, b)));
                break;
            }
            case OP_LT: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if ((IS_INT(a) || IS_FLOAT(a)) &&
                    (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da < db));
                } else {
                    vm_runtime_error(vm, "LT on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_LE: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if ((IS_INT(a) || IS_FLOAT(a)) &&
                    (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da <= db));
                } else {
                    vm_runtime_error(vm, "LE on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GT: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if ((IS_INT(a) || IS_FLOAT(a)) &&
                    (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da > db));
                } else {
                    vm_runtime_error(vm, "GT on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GE: {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                if ((IS_INT(a) || IS_FLOAT(a)) &&
                    (IS_INT(b) || IS_FLOAT(b))) {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    vm_push(vm, BOOL_VAL(da >= db));
                } else {
                    vm_runtime_error(vm, "GE on non-numbers");
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

            case OP_NOP:
                break;

            case OP_LOAD_CONST_U16: {
                uint16_t idx = read_u16(ip);
                ip += 2;
                frame->ip = ip;
                if (idx >= bytecode->const_count) {
                    printf("const count: %llu, idx: %d\n", bytecode->const_count, idx);
                    vm_runtime_error(vm, "LOAD_CONST: bad index");
                    return INTERPRET_RUNTIME_ERROR;
                }
                Constant* c = &bytecode->constants[idx];
                Value v = NIL_VAL;
                switch (c->type) {
                    case CONST_INT:   v = INT_VAL(c->int_val); break;
                    case CONST_FLOAT: v = FLOAT_VAL(c->float_val); break;
                    case CONST_STRING: {
                        StringObject* s = vm_copy_string(vm, c->str_val,
                                                         (int)strlen(c->str_val));
                        v = OBJECT_VAL(s);
                        break;
                    }
                    case CONST_CLOSURE: {
                        v = INT_VAL((int64_t)c->closure.func_idx);
                        break;
                    }
                    default:
                        vm_runtime_error(vm, "LOAD_CONST: unsupported const type");
                        return INTERPRET_RUNTIME_ERROR;
                }
                vm_push(vm, v);
                break;
            }

            case OP_LOAD_LOCAL_U8: {
                uint8_t idx = *ip++;
                frame->ip = ip;
                vm_push(vm, vm_load_local(vm, (int)idx));
                break;
            }
            case OP_STORE_LOCAL_U8: {
                uint8_t idx = *ip++;
                frame->ip = ip;
                Value v = vm_peek(vm, 0);
                vm_store_local(vm, (int)idx, v);
                break;
            }

            case OP_LOAD_GLOBAL_U16: {
                uint16_t idx = read_u16(ip);
                ip += 2;
                frame->ip = ip;
                vm_push(vm, vm_load_global(vm, (int)idx));
                break;
            }
            case OP_STORE_GLOBAL_U16: {
                uint16_t idx = read_u16(ip);
                ip += 2;
                frame->ip = ip;
                Value v = vm_peek(vm, 0);
                vm_store_global(vm, (int)idx, v);
                break;
            }

            case OP_JUMP_U16: {
                int16_t offset = (int16_t)read_u16(ip);
                ip += 2;
                ip += offset;
                frame->ip = ip;
                break;
            }
            case OP_JUMP_IF_FALSE_U16: {
                int16_t offset = (int16_t)read_u16(ip);
                ip += 2;
                Value cond = vm_peek(vm, 0);
                if (!value_to_bool(cond)) {
                    ip += offset;
                }
                frame->ip = ip;
                break;
            }
            case OP_JUMP_IF_TRUE_U16: {
                int16_t offset = (int16_t)read_u16(ip);
                ip += 2;
                Value cond = vm_peek(vm, 0);
                if (value_to_bool(cond)) {
                    ip += offset;
                }
                frame->ip = ip;
                break;
            }

            case OP_CALL_U8:
            case OP_CALL_U16: {
                int argc;
                if (opcode == OP_CALL_U8) {
                    argc = *ip++;
                } else {
                    argc = (int)read_u16(ip);
                    ip += 2;
                }
                frame->ip = ip;

                Value callee = vm_peek(vm, argc);

                if (IS_NATIVE(callee)) {
                    // Снять аргументы в массив
                    Value* args = NULL;
                    if (argc > 0) {
                        args = ALLOCATE(Value, argc);
                        for (int i = argc - 1; i >= 0; i--) {
                            args[i] = vm_pop(vm);
                        }
                    }
                    vm_pop(vm);

                    NativeFunctionObject* nf = AS_NATIVE(callee);
                    Value result = nf->function(argc, args);

                    if (argc > 0) FREE_ARRAY(Value, args, argc);

                    vm_push(vm, result);
                } else if (IS_CLOSURE(callee)) {
                    ClosureObject* cl = AS_CLOSURE(callee);
                    FunctionObject* fn = cl->function;

                    // Снимаем аргументы и callee одним блоком
                    // стек сейчас: [..., callee, arg0, ..., argN-1]
                    Value* args = NULL;
                    if (argc > 0) {
                        args = ALLOCATE(Value, argc);
                        for (int i = argc - 1; i >= 0; i--) {
                            args[i] = vm_pop(vm);
                        }
                    }
                    vm_pop(vm); // callee

                    // создаём новый фрейм
                    vm_push_frame_with_ip(vm,
                                          fn->bytecode,
                                          fn->local_count,
                                          fn->bytecode->code + fn->bytecode->functions[fn->func_index].code_start);

                    CallFrame* new_frame = &vm->frames[vm->frame_count - 1];
                    new_frame->closure = cl;

                    // args → локалы 0..argc-1
                    for (int i = 0; i < argc; i++) {
                        vm_store_local(vm, i, args[i]);
                    }
                    if (argc > 0) FREE_ARRAY(Value, args, argc);

                    // переключаемся на новый фрейм
                    frame = new_frame;
                    ip = frame->ip;
                } else {
                    vm_runtime_error(vm, "Attempt to call non-callable value");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_RETURN: {
                Value result = vm_pop(vm);
                CallFrame* old_frame = &vm->frames[vm->frame_count - 1];

                vm->sp = old_frame->slots_offset;

                vm_pop_frame(vm);

                if (vm->frame_count == 0) {
                    vm_push(vm, result);
                    return INTERPRET_OK;
                }

                vm_push(vm, result);
                // восстановить текущий фрейм и ip
                frame = &vm->frames[vm->frame_count - 1];
                ip = frame->ip;
                break;
            }

            case OP_RETURN_NIL: {
                CallFrame* old_frame = &vm->frames[vm->frame_count - 1];

                vm->sp = old_frame->slots_offset;

                vm_pop_frame(vm);

                if (vm->frame_count == 0) {
                    vm_push(vm, NIL_VAL);
                    return INTERPRET_OK;
                }

                vm_push(vm, NIL_VAL);
                frame = &vm->frames[vm->frame_count - 1];
                ip = frame->ip;
                break;
            }

            case OP_ARRAY_NEW_U8: {
                uint8_t count = *ip++;
                frame->ip = ip;
                ArrayObject* arr = vm_new_array(vm, count);
                for (int i = count - 1; i >= 0; i--) {
                    arr->items[i] = vm_pop(vm);
                    arr->count++;
                }
                vm_push(vm, OBJECT_VAL(arr));
                break;
            }
            case OP_ARRAY_GET: {
                Value index = vm_pop(vm);
                Value array = vm_pop(vm);
                if (!IS_ARRAY(array) || !IS_INT(index)) {
                    vm_runtime_error(vm, "ARRAY_GET type error");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ArrayObject* arr = AS_ARRAY(array);
                int idx = (int)AS_INT(index);
                if (idx < 0 || idx >= arr->count) {
                    vm_runtime_error(vm, "ARRAY_GET out of bounds");
                    return INTERPRET_RUNTIME_ERROR;
                }
                vm_push(vm, arr->items[idx]);
                break;
            }
            case OP_ARRAY_SET: {
                Value value = vm_pop(vm);
                Value index = vm_pop(vm);
                Value array = vm_pop(vm);
                if (!IS_ARRAY(array) || !IS_INT(index)) {
                    vm_runtime_error(vm, "ARRAY_SET type error");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ArrayObject* arr = AS_ARRAY(array);
                int idx = (int)AS_INT(index);
                if (idx < 0 || idx >= arr->count) {
                    vm_runtime_error(vm, "ARRAY_SET out of bounds");
                    return INTERPRET_RUNTIME_ERROR;
                }
                arr->items[idx] = value;
                vm_push(vm, value);
                break;
            }
            case OP_ARRAY_LEN: {
                Value array = vm_pop(vm);
                if (!IS_ARRAY(array)) {
                    vm_runtime_error(vm, "ARRAY_LEN on non-array");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ArrayObject* arr = AS_ARRAY(array);
                vm_push(vm, INT_VAL(arr->count));
                break;
            }

            case OP_PRINT: {
                Value v = vm_pop(vm);
                vm_print_value(v);
                printf("\n");
                break;
            }

            case OP_DUP: {
                Value v = vm_peek(vm, 0);
                vm_push(vm, v);
                break;
            }

            case OP_POP: {
                (void)vm_pop(vm);
                break;
            }

            case OP_BREAK: {
                break;
            }

            case OP_HALT: {
                return INTERPRET_OK;
            }

            case OP_NEW_CLOSURE: {
                frame->ip = ip;

                Value funVal = vm_pop(vm);
                if (!IS_INT(funVal)) {
                    vm_runtime_error(vm, "NEW_CLOSURE: expected function index on stack");
                    return INTERPRET_RUNTIME_ERROR;
                }
                uint16_t func_idx = (uint16_t)AS_INT(funVal);

                if (func_idx >= bytecode->func_count) {
                    vm_runtime_error(vm, "NEW_CLOSURE: bad func index");
                    return INTERPRET_RUNTIME_ERROR;
                }

                Function* fmeta = &bytecode->functions[func_idx];

                FunctionObject* fn_obj = vm_new_function(
                    vm,
                    fmeta->name ? fmeta->name : "<fn>",
                    fmeta->arity,
                    fmeta->max_locals,
                    fmeta->n_upvalues,
                    bytecode,
                    (int)func_idx
                );

                ClosureObject* clo = vm_new_closure(vm, fn_obj);

                for (int i = 0; i < fmeta->n_upvalues; i++) {
                    UpvalueInfo* uinfo = &fmeta->upvalues[i];
                    if (uinfo->is_local) {
                        Value* slot = &vm->stack[frame->slots_offset + uinfo->location];
                        Upvalue* uv = ALLOCATE(Upvalue, 1);
                        link_object(vm, (Object*)uv, VAL_CLOSURE); // пока так
                        uv->location = slot;
                        uv->closed = NIL_VAL;
                        uv->next = NULL;
                        clo->upvalues[i] = uv;
                    } else {
                        if (!frame->closure || uinfo->location >= frame->closure->upvalue_count) {
                            vm_runtime_error(vm, "NEW_CLOSURE: bad upvalue chain");
                            return INTERPRET_RUNTIME_ERROR;
                        }
                        clo->upvalues[i] = frame->closure->upvalues[uinfo->location];
                    }
                }

                vm_push(vm, OBJECT_VAL(clo));
                break;
            }

            case OP_NEW_CLASS: {
                uint16_t class_idx = read_u16(ip);
                ip += 2;
                frame->ip = ip;

                if (class_idx >= bytecode->class_count) {
                    vm_runtime_error(vm, "NEW_CLASS: bad class index");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ClassTemplate* tmpl = &bytecode->classes[class_idx];

                StringObject* name = vm_copy_string(vm, "Class", 5);
                ClassObject* klass = vm_new_class(
                    vm,
                    name,
                    tmpl->n_fields,
                    tmpl->n_methods
                );

                for (int i = 0; i < tmpl->n_fields; i++) {
                    if (tmpl->field_names && tmpl->field_names[i]) {
                        int len = (int)strlen(tmpl->field_names[i]);
                        klass->field_names[i] = vm_copy_string(vm, tmpl->field_names[i], len);
                    }
                }

                for (int i = tmpl->n_methods - 1; i >= 0; i--) {
                    Value m = vm_pop(vm);
                    if (!IS_CLOSURE(m)) {
                        vm_runtime_error(vm, "NEW_CLASS: method is not closure");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    klass->methods[i] = AS_CLOSURE(m);
                }

                vm_push(vm, OBJECT_VAL(klass));
                break;
            }

            case OP_LOAD_FIELD_U8: {
                uint8_t idx = *ip++;
                frame->ip = ip;
                Value instance_val = vm_pop(vm);
                if (!IS_INSTANCE(instance_val)) {
                    vm_runtime_error(vm, "LOAD_FIELD: receiver is not instance");
                    return INTERPRET_RUNTIME_ERROR;
                }
                InstanceObject* inst = AS_INSTANCE(instance_val);
                if (idx >= (uint8_t)inst->field_count) {
                    vm_runtime_error(vm, "LOAD_FIELD: bad field index");
                    return INTERPRET_RUNTIME_ERROR;
                }
                vm_push(vm, inst->fields[idx]);
                break;
            }

            case OP_STORE_FIELD_U8: {
                uint8_t idx = *ip++;
                frame->ip = ip;
                Value value = vm_pop(vm);
                Value instance_val = vm_pop(vm);
                if (!IS_INSTANCE(instance_val)) {
                    vm_runtime_error(vm, "STORE_FIELD: receiver is not instance");
                    return INTERPRET_RUNTIME_ERROR;
                }
                InstanceObject* inst = AS_INSTANCE(instance_val);
                if (idx >= (uint8_t)inst->field_count) {
                    vm_runtime_error(vm, "STORE_FIELD: bad field index");
                    return INTERPRET_RUNTIME_ERROR;
                }
                inst->fields[idx] = value;
                vm_push(vm, value);
                break;
            }

            case OP_CALL_METHOD_U8: {
                uint8_t method_idx = *ip++;
                frame->ip = ip;

                int argc = 0; // FIXME synchronize with bytecode codegen

                Value receiver = vm_peek(vm, argc);
                if (!IS_INSTANCE(receiver)) {
                    vm_runtime_error(vm, "CALL_METHOD: receiver is not instance");
                    return INTERPRET_RUNTIME_ERROR;
                }
                InstanceObject* inst = AS_INSTANCE(receiver);
                ClassObject* klass = inst->klass;
                if (!klass || method_idx >= (uint8_t)klass->method_count) {
                    vm_runtime_error(vm, "CALL_METHOD: bad method index");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ClosureObject* method_clo = klass->methods[method_idx];
                if (!method_clo) {
                    vm_runtime_error(vm, "CALL_METHOD: null method");
                    return INTERPRET_RUNTIME_ERROR;
                }

                vm_push(vm, receiver);
                vm_call_value(vm, OBJECT_VAL(method_clo), argc + 1);
                break;
            }

            case OP_LOAD_UPVALUE_U8: {
                uint8_t idx = *ip++;
                frame->ip = ip;
                if (!frame->closure || idx >= (uint8_t)frame->closure->upvalue_count) {
                    vm_runtime_error(vm, "LOAD_UPVALUE: no closure or bad index");
                    return INTERPRET_RUNTIME_ERROR;
                }
                Upvalue* uv = frame->closure->upvalues[idx];
                vm_push(vm, uv ? *uv->location : NIL_VAL);
                break;
            }

            case OP_STORE_UPVALUE_U8: {
                uint8_t idx = *ip++;
                frame->ip = ip;
                if (!frame->closure || idx >= (uint8_t)frame->closure->upvalue_count) {
                    vm_runtime_error(vm, "STORE_UPVALUE: no closure or bad index");
                    return INTERPRET_RUNTIME_ERROR;
                }
                Upvalue* uv = frame->closure->upvalues[idx];
                if (!uv || !uv->location) {
                    vm_runtime_error(vm, "STORE_UPVALUE: null location");
                    return INTERPRET_RUNTIME_ERROR;
                }
                Value v = vm_peek(vm, 0);
                *uv->location = v;
                break;
            }
            default:
                vm_runtime_error(vm, "Unknown opcode %d", opcode);
                return INTERPRET_RUNTIME_ERROR;
        }
    }

    return INTERPRET_OK;
}

void vm_mark_roots(VM* vm) {
    (void)vm;
}

void vm_collect_garbage(VM* vm) {
    (void)vm;
}