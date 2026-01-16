#include "vm.h"
#include "core/bytecode.h"
#include "utils/memory.h"
#include "gc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>

#include "jit.h"
#include "jit_vm_bridge.h"

#define INITIAL_STACK_SIZE 512
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

    printf("%04zu  %s", offset, opcode_name(opcode));

    switch (op) {
        case OP_LOAD_CONST_U16: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            printf(" %u", idx);
            if (idx < bc->const_count) {
                Constant* c = &bc->constants[idx];
                printf(" (");
                switch (c->type) {
                    case CONST_INT:    printf("INT %lld", (long long)c->int_val); break;
                    case CONST_FLOAT:  printf("FLOAT %g", c->float_val); break;
                    case CONST_STRING: printf("STRING \"%s\"", c->str_val); break;
                    default:           printf("?"); break;
                }
                printf(")");
            }
            break;
        }

        case OP_LOAD_LOCAL_U8:
        case OP_STORE_LOCAL_U8:
        case OP_LOAD_UPVALUE_U8:
        case OP_STORE_UPVALUE_U8:
        case OP_ARRAY_NEW_U8: {
            uint8_t b = ip[1];
            printf(" %u", b);
            break;
        }

        case OP_JUMP_U16:
        case OP_JUMP_IF_FALSE_U16:
        case OP_JUMP_IF_TRUE_U16: {
            int16_t off = (int16_t)(ip[1] << 8 | ip[2]);
            size_t target = offset + 3 + off;
            printf(" %d -> %zu", (int)off, target);
            break;
        }

        case OP_CALL_U8: {
            uint8_t argc = ip[1];
            printf(" argc=%u", argc);
            break;
        }
        case OP_CALL_U16: {
            uint16_t argc = (uint16_t)(ip[1] << 8 | ip[2]);
            printf(" argc=%u", argc);
            break;
        }

        case OP_LOAD_GLOBAL_U16:
        case OP_STORE_GLOBAL_U16: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            printf(" %u", idx);
            break;
        }

        case OP_NEW_CLOSURE: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            printf(" %u", idx);
            break;
        }

        default:
            break;
    }

    printf("\n");
}


static void debug_print_stack(VM* vm) {
    printf("  Stack (size=%d) sp=%d:\n", vm->stack_size, vm->sp);
    for (int i = vm->sp - 1; i >= 0; i--) {
        printf("    [%2d] = ", i);
        debug_print_value(vm->stack[i]);
        printf("\n");
    }
}

static void debug_print_frames(VM* vm) {
    printf("  Frames (count=%d):\n", vm->frame_count);
    for (int i = vm->frame_count - 1; i >= 0; i--) {
        CallFrame* f = &vm->frames[i];
        int ip = (int)(f->ip - f->bytecode->code);
        printf(
                "    frame[%d]: func_idx=%d ip=%d slots_offset=%d slot_count=%d",
                i,
                f->closure ? f->closure->function->func_index : -1,
                ip,
                f->slots_offset,
                f->slot_count);
        if (f->closure && f->closure->function && f->closure->function->name) {
            printf(" name=%s", f->closure->function->name);
        }
        printf("\n");
    }
}

static void debug_print_locals(VM* vm) {
    if (vm->frame_count == 0) return;
    CallFrame* f = &vm->frames[vm->frame_count - 1];
    printf("  Locals (frame top, count=%d):\n", f->slot_count);
    for (int i = 0; i < f->slot_count; i++) {
        int idx = f->slots_offset + i;
        printf("    local[%2d] = ", i);
        debug_print_value(vm->stack[idx]);
        printf("\n");
    }
}

static void debug_print_globals(VM* vm) {
    printf("  Globals (count=%d):\n", vm->global_count);
    for (int i = 0; i < vm->global_count; i++) {
        printf("    g[%2d] = ", i);
        debug_print_value(vm->globals[i]);
        printf("\n");
    }
}

static void debug_print_upvalues(VM* vm) {
    if (vm->frame_count == 0) return;
    CallFrame* f = &vm->frames[vm->frame_count - 1];
    if (!f->closure) return;

    ClosureObject* clo = f->closure;
    printf("  Upvalues (count=%d):\n", clo->upvalue_count);
    for (int i = 0; i < clo->upvalue_count; i++) {
        Upvalue* uv = clo->upvalues[i];
        printf("    uv[%2d] = ", i);
        if (!uv) {
            printf("<null>\n");
        } else if (uv->location) {
            debug_print_value(*uv->location);
            printf(" (open)\n");
        } else {
            debug_print_value(uv->closed);
            printf(" (closed)\n");
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

int value_to_bool(Value* value) {
    switch (value->type) {
        case VAL_NIL:   return 0;
        case VAL_BOOL:  return value->as.boolean;
        case VAL_INT:   return value->as.integer != 0;
        case VAL_FLOAT: return value->as.floating != 0.0;
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
        case VAL_UPVALUE:  return "upvalue";
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
        default:           printf("[unknown]"); break;
    }
}

VM* vm_new(void) {
    VM* vm = ALLOCATE(VM, 1);
    memset(vm, 0, sizeof(VM));

    vm->stack_capacity = INITIAL_STACK_SIZE;
    vm->stack = ALLOCATE(Value, vm->stack_capacity);
    vm->sp = 0;
    vm->stack_size = 0;

    for (int i = 0; i < vm->stack_capacity; i++) {
        vm->stack[i] = NIL_VAL;
    }

    vm->frame_capacity = INITIAL_FRAMES;
    vm->frames = ALLOCATE(CallFrame, vm->frame_capacity);
    vm->frame_count = 0;

    vm->global_capacity = INITIAL_GLOBALS;
    vm->globals = ALLOCATE(Value, vm->global_capacity);
    vm->global_count = 0;

    for (int i = 0; i < vm->global_capacity; i++) {
        vm->globals[i] = NIL_VAL;
    }

    vm->constants.values = ALLOCATE(Value, INITIAL_CONSTANTS);
    vm->constants.capacity = INITIAL_CONSTANTS;
    vm->constants.count = 0;

    for (int i = 0; i < vm->constants.capacity; i++) {
        vm->constants.values[i] = NIL_VAL;
    }

    vm->objects = NULL;
    vm->bytes_allocated = 0;
    vm->next_gc = 1024 * 1024;
    vm->gc_collecting = 0;
    vm->gc_enabled = 1;
    vm->debug_gc = 0;
    vm->open_upvalues = NULL;

    vm->error_count = 0;
    vm->error_message = NULL;
    vm->line = 0;
    vm->debug = 0;
    vm->debug_level = DEBUG_NONE;
    vm->exit_code = 0;

    vm->jit = NULL;

    return vm;
}

void vm_free(VM* vm) {
    if (!vm) return;

    gc_collect_garbage(vm);

    if (vm->jit && vm->jit->debug) {
        jit_print_stats(vm->jit);
    }

    FREE_ARRAY(Value, vm->stack, vm->stack_capacity);
    FREE_ARRAY(CallFrame, vm->frames, vm->frame_capacity);
    FREE_ARRAY(Value, vm->globals, vm->global_capacity);
    FREE_ARRAY(Value, vm->constants.values, vm->constants.capacity);

    jit_vm_cleanup(vm);

    free_ptr(vm);
}

void vm_push(VM* vm, Value value) {
    if (vm->sp >= vm->stack_capacity) {
        int old = vm->stack_capacity;
        int new_cap = old * 2;
        Value* old_stack = vm->stack;
        vm->stack = GROW_ARRAY(Value, vm->stack, old, new_cap);
        vm->stack_capacity = new_cap;

        if (vm->stack != old_stack) {
            ptrdiff_t offset = vm->stack - old_stack;
            for (Upvalue* uv = vm->open_upvalues; uv != NULL; uv = uv->next) {
                if (uv->location && uv->location != &uv->closed) {
                    uv->location = uv->location + offset;
                }
            }
        }
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

void vm_push_frame_with_ip(VM* vm, Bytecode* bytecode,
                           int slot_count, uint8_t* ip_start) {
    vm_push_frame_with_ip_at(vm, bytecode, slot_count, ip_start, vm->sp);
}

void vm_push_frame_with_ip_at(VM* vm, Bytecode* bytecode,
                              int slot_count, uint8_t* ip_start,
                              int slots_offset) {
    if (vm->frame_count >= vm->frame_capacity) {
        int old = vm->frame_capacity;
        int new_cap = old * 2;
        vm->frames = GROW_ARRAY(CallFrame, vm->frames, old, new_cap);
        vm->frame_capacity = new_cap;
    }

    CallFrame* frame = &vm->frames[vm->frame_count++];
    frame->bytecode = bytecode;
    frame->ip = ip_start;

    frame->slots_offset = slots_offset;
    frame->slot_count = slot_count;
    frame->closure = NULL;

    if (vm->sp < slots_offset) vm->sp = slots_offset;

    int needed = slots_offset + slot_count;
    while (vm->sp < needed) {
        vm_push(vm, NIL_VAL);
    }
}

void vm_pop_frame(VM* vm) {
    if (vm->frame_count == 0) return;

    CallFrame* frame = &vm->frames[vm->frame_count - 1];
    int frame_start = frame->slots_offset;
    int old_sp = vm->sp;

    if (vm->debug) {
        printf("[POP_FRAME] frame %d: slots_offset=%d, sp=%d\n",
               vm->frame_count - 1, frame_start, old_sp);
    }

    vm->sp = frame_start;

    vm_close_upvalues(vm, &vm->stack[frame_start]);

    vm->frame_count--;

    if (vm->debug) {
        printf("[POP_FRAME] New sp=%d, frame_count=%d\n\n", vm->sp, vm->frame_count);
    }
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
    CallFrame* frame = &vm->frames[vm->frame_count - 1];

    int stack_index = frame->slots_offset + index;
    Value result = vm->stack[stack_index];
    return result;
}

void vm_runtime_error(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    printf("Runtime error: ");
    vfprintf(stderr, format, args);
    printf("\n");
    va_end(args);

    vm->error_count++;
    vm->exit_code = 1;
}

StringObject* vm_copy_string(VM* vm, const char* chars, int length) {
    char* copy = ALLOCATE(char, length + 1);
    memcpy(copy, chars, (size_t)length);
    copy[length] = '\0';

    StringObject* str = ALLOCATE(StringObject, 1);

    str->chars = copy;
    str->length = length;
    str->hash = hash_string(copy, length);

    vm_register_bytes(vm, sizeof(StringObject) + (size_t)(length + 1));

    link_object(vm, (Object*)str, VAL_STRING);

    gc_check(vm);

    return str;
}

StringObject* vm_take_string(VM* vm, char* chars, int length) {
    if (chars == NULL || length < 0) {
        return NULL;
    }

    StringObject* str = ALLOCATE(StringObject, 1);

    str->chars = chars;
    str->length = length;
    str->hash = hash_string(chars, length);

    vm_register_bytes(vm, sizeof(StringObject));

    link_object(vm, (Object*)str, VAL_STRING);

    gc_check(vm);

    return str;
}

ArrayObject* vm_new_array(VM* vm, int capacity) {
    ArrayObject* arr = (ArrayObject*)vm_realloc(
            vm, NULL, 0, sizeof(ArrayObject));

    arr->capacity = capacity > 0 ? capacity : 0;
    arr->count = 0;

    link_object(vm, (Object*)arr, VAL_ARRAY);

    if (arr->capacity > 0) {
        arr->items = (Value*)vm_realloc(
                vm, NULL, 0, sizeof(Value) * (size_t)arr->capacity);
    } else {
        arr->items = NULL;
    }

    gc_check(vm);

    return arr;
}

void vm_array_append(VM* vm, ArrayObject* array, Value value) {
    if (!array) return;

    if (array->count >= array->capacity) {
        int old_cap = array->capacity;
        int new_cap = old_cap < 8 ? 8 : old_cap * 2;

        array->items = (Value*)vm_realloc(
                vm,
                array->items,
                sizeof(Value) * (size_t)old_cap,
                sizeof(Value) * (size_t)new_cap
        );

        array->capacity = new_cap;
    }

    array->items[array->count++] = value;

    gc_check(vm);
}

NativeFunctionObject* vm_new_native_function(VM* vm, const char* name, NativeFn function, int arity) {
    NativeFunctionObject* nf = (NativeFunctionObject*)vm_realloc(
            vm, NULL, 0, sizeof(NativeFunctionObject));

    nf->function = function;
    nf->arity = arity;

    int len = (int)strlen(name);

    nf->name = (char*)vm_realloc(vm, NULL, 0, (size_t)len + 1);

    memcpy(nf->name, name, (size_t)len + 1);

    link_object(vm, (Object*)nf, VAL_NATIVE_FN);

    gc_check(vm);

    return nf;
}

FunctionObject* vm_new_function(VM* vm,
                               const char* name,
                               int arity,
                               int local_count,
                               int upvalue_count,
                               Bytecode* bytecode,
                               int func_index) {
    FunctionObject* fn = (FunctionObject*)vm_realloc(
        vm, NULL, 0, sizeof(FunctionObject));

    int len = (int)strlen(name);

    fn->name = (char*)vm_realloc(vm, NULL, 0, (size_t)len + 1);

    memcpy(fn->name, name, (size_t)len + 1);
    fn->arity = arity;
    fn->local_count = local_count;
    fn->upvalue_count = upvalue_count;
    fn->bytecode = bytecode;
    fn->func_index = func_index;

    link_object(vm, (Object*)fn, VAL_FUNCTION);

    return fn;
}

ClosureObject* vm_new_closure(VM* vm, FunctionObject* function) {
    uint8_t upvalue_count = function->upvalue_count;

    ClosureObject* cl = (ClosureObject*)vm_realloc(
        vm, NULL, 0, sizeof(ClosureObject));

    cl->function = function;
    cl->upvalue_count = upvalue_count;

    if (cl->upvalue_count > 0) {
        cl->upvalues = (Upvalue**)vm_realloc(
            vm, NULL, 0, sizeof(Upvalue*) * (size_t)cl->upvalue_count);

        for (int i = 0; i < cl->upvalue_count; i++) {
            cl->upvalues[i] = NULL;
        }
    } else {
        cl->upvalues = NULL;
    }

    link_object(vm, (Object*)cl, VAL_CLOSURE);

    return cl;
}

Value vm_call_native(VM* vm, NativeFn function, int arg_count) {
    int callee_index = vm->sp - 1 - arg_count;
    Value* args = NULL;
    if (arg_count > 0) {
        args = &vm->stack[callee_index + 1];
    }

    Value result = function(vm, arg_count, args);

    vm->sp = callee_index;
    vm_push(vm, result);
    return result;
}

Upvalue* vm_capture_upvalue(VM* vm, Value* slot) {
    Upvalue* prev = NULL;
    Upvalue* uv = vm->open_upvalues;

    while (uv && uv->location > slot) {
        prev = uv;
        uv = uv->next;
    }

    if (uv && uv->location == slot) {
        return uv;
    }

    Upvalue* created = (Upvalue*)vm_realloc(vm, NULL, 0, sizeof(Upvalue));

    created->location = slot;
    created->closed = NIL_VAL;
    created->next = uv;

    if (prev) prev->next = created;
    else vm->open_upvalues = created;

    link_object(vm, (Object*)created, VAL_UPVALUE);

    return created;
}

void vm_close_upvalues(VM* vm, Value* last) {
    while (vm->open_upvalues && vm->open_upvalues->location >= last) {
        Upvalue* uv = vm->open_upvalues;
        uv->closed = *uv->location;
        uv->location = &uv->closed;
        vm->open_upvalues = uv->next;
    }
}

uint16_t read_u16(uint8_t* ip) {
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
            default:
                v = NIL_VAL;
                break;
        }

        vm->constants.values[i] = v;
    }
}

int vm_op_add(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);

    if (IS_STRING(a) && IS_STRING(b)) {
        StringObject* sa = AS_STRING(a);
        StringObject* sb = AS_STRING(b);
        int new_len = sa->length + sb->length;
        char* chars = (char*)vm_realloc(vm, NULL, 0, (size_t)new_len + 1);
        memcpy(chars, sa->chars, (size_t)sa->length);
        memcpy(chars + sa->length, sb->chars, (size_t)sb->length);
        chars[new_len] = '\0';
        StringObject* new_str = vm_take_string(vm, chars, new_len);
        vm_push(vm, OBJECT_VAL(new_str));
    }
    else if (IS_INT(a) && IS_INT(b)) {
        vm_push(vm, INT_VAL(AS_INT(a) + AS_INT(b)));
    }
    else if ((IS_INT(a) || IS_FLOAT(a)) &&
             (IS_INT(b) || IS_FLOAT(b))) {
        double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
        double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
        vm_push(vm, FLOAT_VAL(da + db));
    }
    else {
        vm_runtime_error(vm, "ADD on non-numbers or non-strings");
        return 1;
    }
    return 0;
}

int vm_op_sub(VM* vm) {
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
                    return 1;
               }
    return 0;
}

int vm_op_mul(VM* vm) {
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
                    return 1;
               }
    return 0;
}

int vm_op_div(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);

    if (!((IS_INT(a) || IS_FLOAT(a)) &&
          (IS_INT(b) || IS_FLOAT(b)))) {
                vm_runtime_error(vm, "DIV on non-numbers");
                return 1;
          }

    if (IS_INT(a) && IS_INT(b)) {
        int64_t ia = AS_INT(a);
        int64_t ib = AS_INT(b);
        if (ib == 0) {
            vm_runtime_error(vm, "Division by zero");
            return 1;
        }
        vm_push(vm, INT_VAL(ia / ib));
    } else {
        double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
        double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
        if (db == 0.0) {
            vm_runtime_error(vm, "Division by zero");
            return 1;
        }
        vm_push(vm, FLOAT_VAL(da / db));
    }
    return 0;
}

int vm_op_mod(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    if (IS_INT(a) && IS_INT(b)) {
        int64_t bb = AS_INT(b);
        if (bb == 0) {
            vm_runtime_error(vm, "Modulo by zero");
            return 1;
        }
        vm_push(vm, INT_VAL(AS_INT(a) % bb));
    } else {
        vm_runtime_error(vm, "MOD on non-integers");
        return 1;
    }
    return 0;
}

int vm_op_neg(VM* vm) {
    Value v = vm_pop(vm);
    if (IS_INT(v)) {
        vm_push(vm, INT_VAL(-AS_INT(v)));
    } else if (IS_FLOAT(v)) {
        vm_push(vm, FLOAT_VAL(-AS_FLOAT(v)));
    } else {
        vm_runtime_error(vm, "NEG on non-number");
        return 1;
    }
    return 0;
}

int vm_op_not(VM* vm) {
    Value v = vm_pop(vm);
    vm_push(vm, BOOL_VAL(!value_to_bool(&v)));
    return 0;
}


int vm_op_dup(VM* vm) {
    Value v = vm_peek(vm, 0);
    vm_push(vm, v);
    return 0;
}

int vm_op_eq(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    vm_push(vm, BOOL_VAL(vm_values_equal(a, b)));
    return 0;
}

int vm_op_neq(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    vm_push(vm, BOOL_VAL(!vm_values_equal(a, b)));
    return 0;
}

int vm_op_lt(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    if ((IS_INT(a) || IS_FLOAT(a)) &&
        (IS_INT(b) || IS_FLOAT(b))) {
            double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
            double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
            vm_push(vm, BOOL_VAL(da < db));
        } else {
            vm_runtime_error(vm, "LT on non-numbers");
            return 1;
        }
    return 0;
}

int vm_op_le(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    if ((IS_INT(a) || IS_FLOAT(a)) &&
        (IS_INT(b) || IS_FLOAT(b))) {
            double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
            double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
            vm_push(vm, BOOL_VAL(da <= db));
        } else {
            vm_runtime_error(vm, "LE on non-numbers");
            return 1;
        }
    return 0;
}

int vm_op_gt(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    if ((IS_INT(a) || IS_FLOAT(a)) &&
        (IS_INT(b) || IS_FLOAT(b))) {
            double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
            double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
            vm_push(vm, BOOL_VAL(da > db));
        } else {
            vm_runtime_error(vm, "GT on non-numbers");
            return 1;
        }
    return 0;
}

int vm_op_ge(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    if ((IS_INT(a) || IS_FLOAT(a)) &&
        (IS_INT(b) || IS_FLOAT(b))) {
            double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
            double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
            vm_push(vm, BOOL_VAL(da >= db));
        } else {
            vm_runtime_error(vm, "GE on non-numbers");
            return 1;
        }
    return 0;
}

int vm_op_and(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    vm_push(vm, BOOL_VAL(value_to_bool(&a) && value_to_bool(&b)));
    return 0;
}

int vm_op_or(VM* vm) {
    Value b = vm_pop(vm);
    Value a = vm_pop(vm);
    vm_push(vm, BOOL_VAL(value_to_bool(&a) || value_to_bool(&b)));
    return 0;
}

int vm_op_load_const_u16(VM* vm, Bytecode* bytecode, CallFrame* frame, uint8_t* ip) {
    uint16_t idx = read_u16(ip);
    ip += 2;
    frame->ip = ip;
    if (idx >= bytecode->const_count) {
        vm_runtime_error(vm, "LOAD_CONST: bad index");
        return 1;
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
            v = INT_VAL((int64_t)idx);
            break;
        }
        default:
            vm_runtime_error(vm, "LOAD_CONST: unsupported const type");
            return 1;
    }
    vm_push(vm, v);
    return 0;
}


int vm_op_load_local_u8(VM* vm, CallFrame* frame, uint8_t* ip) {
    uint8_t idx = *ip++;
    frame->ip = ip;
    vm_push(vm, vm_load_local(vm, (int)idx));
    return 0;
}

int vm_op_store_local_u8(VM* vm, CallFrame* frame, uint8_t* ip) {
    uint8_t idx = *ip++;
    frame->ip = ip;
    Value v = vm_peek(vm, 0);
    vm_store_local(vm, (int)idx, v);
    return 0;
}

int vm_op_load_upvalue_u8(VM* vm, CallFrame* frame, uint8_t* ip) {
    uint8_t idx = *ip++;
    frame->ip = ip;
    if (!frame->closure || idx >= (uint8_t)frame->closure->upvalue_count) {
        vm_runtime_error(vm, "LOAD_UPVALUE: no closure or bad index");
        return 1;
    }
    Upvalue* uv = frame->closure->upvalues[idx];
    if (!uv) {
        vm_runtime_error(vm, "LOAD_UPVALUE: null upvalue");
        return 1;
    }
    Value val = (uv->location != NULL) ? *uv->location : uv->closed;
    vm_push(vm, val);
    return 0;
}

int vm_op_store_upvalue_u8(VM* vm, CallFrame* frame, uint8_t* ip) {
    uint8_t idx = *ip++;
    frame->ip = ip;
    if (!frame->closure || idx >= (uint8_t)frame->closure->upvalue_count) {
        vm_runtime_error(vm, "STORE_UPVALUE: no closure or bad index");
        return 1;
    }
    Upvalue* uv = frame->closure->upvalues[idx];
    if (!uv || !uv->location) {
        vm_runtime_error(vm, "STORE_UPVALUE: null location");
        return 1;
    }
    Value v = vm_peek(vm, 0);
    *uv->location = v;
    return 0;
}


int vm_op_jump_u16(VM* vm, CallFrame* frame, uint8_t* ip) {
    int16_t offset = (int16_t)read_u16(ip);
    ip += 2;
    ip += offset;
    frame->ip = ip;
    return 0;
}

int vm_op_jump_if_false_u16(VM* vm, CallFrame* frame, uint8_t* ip) {
    int16_t offset = (int16_t)read_u16(ip);
    ip += 2;
    Value cond = vm_pop(vm);
    if (!value_to_bool(&cond)) {
        ip += offset;
    }
    frame->ip = ip;
    return 0;
}

int vm_op_jump_if_true_u16(VM* vm, CallFrame* frame, uint8_t* ip) {
    int16_t offset = (int16_t)read_u16(ip);
    ip += 2;
    Value cond = vm_pop(vm);
    if (value_to_bool(&cond)) {
        ip += offset;
    }
    frame->ip = ip;
    return 0;
}

static int vm_run_frame_to_completion(VM* vm, int target_frame_count, Bytecode* initial_bytecode) {
    (void)initial_bytecode;

    while (vm->frame_count > target_frame_count) {
        if (vm->frame_count == 0) break;
        CallFrame* frame = &vm->frames[vm->frame_count - 1];
        Bytecode* bytecode = frame->bytecode;
        uint8_t* ip = frame->ip;
        uint8_t opcode = *ip++;
        frame->ip = ip;

        int op_res = 0;
        switch ((OpCode)opcode) {
            case OP_ADD: op_res = vm_op_add(vm); break;
            case OP_SUB: op_res = vm_op_sub(vm); break;
            case OP_MUL: op_res = vm_op_mul(vm); break;
            case OP_DIV: op_res = vm_op_div(vm); break;
            case OP_MOD: op_res = vm_op_mod(vm); break;
            case OP_NEG: op_res = vm_op_neg(vm); break;
            case OP_NOT: op_res = vm_op_not(vm); break;
            case OP_EQ:  op_res = vm_op_eq(vm); break;
            case OP_NEQ: op_res = vm_op_neq(vm); break;
            case OP_LT:  op_res = vm_op_lt(vm); break;
            case OP_LE:  op_res = vm_op_le(vm); break;
            case OP_GT:  op_res = vm_op_gt(vm); break;
            case OP_GE:  op_res = vm_op_ge(vm); break;
            case OP_AND: op_res = vm_op_and(vm); break;
            case OP_OR:  op_res = vm_op_or(vm); break;
            case OP_NOP: break;
            case OP_DUP: op_res = vm_op_dup(vm); break;
            case OP_POP: (void)vm_pop(vm); break;
            case OP_LOAD_CONST_U16:
                op_res = vm_op_load_const_u16(vm, bytecode, frame, ip);
                break;
            case OP_LOAD_LOCAL_U8:
                op_res = vm_op_load_local_u8(vm, frame, ip);
                break;
            case OP_STORE_LOCAL_U8:
                op_res = vm_op_store_local_u8(vm, frame, ip);
                break;
            case OP_LOAD_GLOBAL_U16:
                op_res = vm_op_load_global_u16(vm, frame, ip);
                break;
            case OP_STORE_GLOBAL_U16:
                op_res = vm_op_store_global_u16(vm, frame, ip);
                break;
            case OP_JUMP_U16:
                op_res = vm_op_jump_u16(vm, frame, ip);
                break;
            case OP_JUMP_IF_FALSE_U16:
                op_res = vm_op_jump_if_false_u16(vm, frame, ip);
                break;
            case OP_JUMP_IF_TRUE_U16:
                op_res = vm_op_jump_if_true_u16(vm, frame, ip);
                break;
            case OP_CALL_U8:
                op_res = vm_op_call_u8(vm, frame, ip);
                break;
            case OP_CALL_U16:
                op_res = vm_op_call_u16(vm, frame, ip);
                break;
            case OP_RETURN: {
                Value result = vm_pop(vm);
                vm_pop_frame(vm);
                if (vm->frame_count >= target_frame_count) {
                    vm_push(vm, result);
                }
                break;
            }
            case OP_RETURN_NIL: {
                vm_pop_frame(vm);
                if (vm->frame_count >= target_frame_count) {
                    vm_push(vm, NIL_VAL);
                }
                break;
            }
            case OP_ARRAY_NEW_U8:
                op_res = vm_op_array_new_u8(vm, frame, ip);
                break;
            case OP_ARRAY_GET:
                op_res = vm_op_array_get(vm);
                break;
            case OP_ARRAY_SET:
                op_res = vm_op_array_set(vm);
                break;
            case OP_ARRAY_LEN:
                op_res = vm_op_array_len(vm);
                break;
            case OP_PRINT:
                op_res = vm_op_print(vm);
                break;
            case OP_NEW_CLOSURE:
                op_res = vm_op_new_closure(vm, bytecode, frame, ip);
                break;
            case OP_LOAD_UPVALUE_U8:
                op_res = vm_op_load_upvalue_u8(vm, frame, ip);
                break;
            case OP_STORE_UPVALUE_U8:
                op_res = vm_op_store_upvalue_u8(vm, frame, ip);
                break;
            case OP_HALT:
                return 0;
            case OP_BREAK:
                break;
            default:
                vm_runtime_error(vm, "Unknown opcode %d in frame completion", opcode);
                return 1;
        }
        if (op_res != 0) return op_res;
    }
    return 0;
}

int vm_op_call_u8(VM* vm, CallFrame* frame, uint8_t* ip) {
    int argc;
    argc = *ip++;
    frame->ip = ip;

    if (vm->debug) {
        printf(
        "[CALL] opcode=%s argc=%d sp=%d\n","CALL_U8",
        argc, vm->sp);
    }

    if (vm->sp < 1 + argc) {
        vm_runtime_error(vm, "CALL: stack underflow (argc=%d, sp=%d)", argc, vm->sp);
        return 1;
    }

    int callee_index = vm->sp - 1 - argc;
    Value callee = vm->stack[callee_index];

    if (vm->debug) {
        printf(
        "[CALL] callee_index=%d type=[%s]\n",
        callee_index, vm_value_type_name(callee));
    }

    if (IS_NATIVE(callee)) {
        NativeFunctionObject* nf = AS_NATIVE(callee);
        Value* args = argc > 0 ? ALLOCATE(Value, argc) : NULL;

        if (vm->debug) {
            printf("[CALL] allocate native done\n");
        }
        for (int i = 0; i < argc; i++) {
            args[i] = vm->stack[callee_index + 1 + i];
        }
        if (vm->debug) {
            printf("[CALL] native args loaded\n");
        }
        Value result = nf->function(vm, argc, args);
        if (vm->debug) {
            printf("[CALL] native call result done\n");
        }
        if (argc > 0) FREE_ARRAY(Value, args, argc);
        if (vm->debug) {
            printf("[CALL] native free done\n");
        }
        vm->sp = callee_index;
        vm_push(vm, result);
        if (vm->debug) {
            printf("[CALL] native done, new sp=%d\n", vm->sp);
        }
    } else if (IS_CLOSURE(callee)) {
        ClosureObject* cl = AS_CLOSURE(callee);
        FunctionObject* fn = cl->function;

        if (argc != fn->arity) {
            vm_runtime_error(vm, "CALL: wrong arg count (expected=%d, got=%d)",
                                         fn->arity, argc);
            return INTERPRET_RUNTIME_ERROR;
        }

        int slots_offset = callee_index;

        if (vm->debug) {
            printf(
            "[CALL] closure func=%s argc=%d slots_offset=%d\n",
            fn->name ? fn->name : "<fn>",
                                argc, slots_offset);
        }

        for (int i = 0; i < argc; i++) {
            vm->stack[slots_offset + i] = vm->stack[callee_index + 1 + i];
        }

        vm->sp = slots_offset + argc;

        int frame_count_before = vm->frame_count;

        vm_push_frame_with_ip_at(
            vm,
            fn->bytecode,
            fn->local_count,
            fn->bytecode->code + fn->bytecode->functions[fn->func_index].code_start,
            slots_offset
        );

        CallFrame* new_frame = &vm->frames[vm->frame_count - 1];
        new_frame->closure = cl;

        if (vm->debug) {
            printf(
            "[CALL] new frame: func=%s slots_offset=%d slot_count=%d sp=%d\n",
            fn->name ? fn->name : "<fn>",
            new_frame->slots_offset, new_frame->slot_count, vm->sp);
        }

        uint32_t func_idx = (uint32_t)fn->func_index;
        bool handled = jit_vm_handle_call(vm, func_idx, fn->bytecode);

        if (handled) {
            return 0;
        }

        int res = vm_run_frame_to_completion(vm, frame_count_before, fn->bytecode);
        if (res != 0) return res;
    } else {
        vm_runtime_error(vm, "Attempt to call non-callable value");
        return 1;
    }

    if (vm->debug) {
        printf("[DEBUG] CALL end\n");
    }
    return 0;
}

int vm_op_call_u16(VM* vm, CallFrame* frame, uint8_t* ip) {
    int argc;
    argc = (int)read_u16(ip);
    ip += 2;
    frame->ip = ip;

    if (vm->debug) {
        printf(
        "[CALL] opcode=%s argc=%d sp=%d\n","CALL_U16",
        argc, vm->sp);
    }

    if (vm->sp < 1 + argc) {
        vm_runtime_error(vm, "CALL: stack underflow (argc=%d, sp=%d)", argc, vm->sp);
        return 1;
    }

    int callee_index = vm->sp - 1 - argc;
    Value callee = vm->stack[callee_index];

    if (vm->debug) {
        printf(
        "[CALL] callee_index=%d type=[%s]\n",
        callee_index, vm_value_type_name(callee));
    }

    if (IS_NATIVE(callee)) {
        NativeFunctionObject* nf = AS_NATIVE(callee);
        Value* args = argc > 0 ? ALLOCATE(Value, argc) : NULL;

        if (vm->debug) {
            printf("[CALL] allocate native done\n");
        }
        for (int i = 0; i < argc; i++) {
            args[i] = vm->stack[callee_index + 1 + i];
        }
        if (vm->debug) {
            printf("[CALL] native args loaded\n");
        }
        Value result = nf->function(vm, argc, args);
        if (vm->debug) {
            printf("[CALL] native call result done\n");
        }
        if (argc > 0) FREE_ARRAY(Value, args, argc);
        if (vm->debug) {
            printf("[CALL] native free done\n");
        }
        vm->sp = callee_index;
        vm_push(vm, result);
        if (vm->debug) {
            printf("[CALL] native done, new sp=%d\n", vm->sp);
        }
    } else if (IS_CLOSURE(callee)) {
        ClosureObject* cl = AS_CLOSURE(callee);
        FunctionObject* fn = cl->function;

        if (argc != fn->arity) {
            vm_runtime_error(vm, "CALL: wrong arg count (expected=%d, got=%d)",
                                         fn->arity, argc);
            return INTERPRET_RUNTIME_ERROR;
        }

        int slots_offset = callee_index;

        if (vm->debug) {
            printf(
            "[CALL] closure func=%s argc=%d slots_offset=%d\n",
            fn->name ? fn->name : "<fn>",
                                argc, slots_offset);
        }

        for (int i = 0; i < argc; i++) {
            vm->stack[slots_offset + i] = vm->stack[callee_index + 1 + i];
        }

        vm->sp = slots_offset + argc;

        int frame_count_before = vm->frame_count;

        vm_push_frame_with_ip_at(
            vm,
            fn->bytecode,
            fn->local_count,
            fn->bytecode->code + fn->bytecode->functions[fn->func_index].code_start,
            slots_offset
        );

        CallFrame* new_frame = &vm->frames[vm->frame_count - 1];
        new_frame->closure = cl;

        if (vm->debug) {
            printf(
            "[CALL] new frame: func=%s slots_offset=%d slot_count=%d sp=%d\n",
            fn->name ? fn->name : "<fn>",
            new_frame->slots_offset, new_frame->slot_count, vm->sp);
        }

        uint32_t func_idx = (uint32_t)fn->func_index;
        bool handled = jit_vm_handle_call(vm, func_idx, fn->bytecode);

        if (handled) {
            return 0;
        }

        int res = vm_run_frame_to_completion(vm, frame_count_before, fn->bytecode);
        if (res != 0) return res;
    } else {
        vm_runtime_error(vm, "Attempt to call non-callable value");
        return 1;
    }

    if (vm->debug) {
        printf("[DEBUG] CALL end\n");
    }
    return 0;
}

int vm_op_new_closure(VM* vm, Bytecode* bytecode, CallFrame* frame, uint8_t* ip) {
    frame->ip = ip;

    Value constVal = vm_pop(vm);
    if (!IS_INT(constVal)) {
        vm_runtime_error(vm, "NEW_CLOSURE: expected const index on stack");
        return 1;
    }

    int64_t raw = AS_INT(constVal);
    if (raw < 0) {
        vm_runtime_error(vm, "NEW_CLOSURE: negative const index");
        return 1;
    }

    size_t const_index = (size_t)raw;
    if (const_index >= bytecode->const_count) {
        vm_runtime_error(vm, "NEW_CLOSURE: bad const index");
        return 1;
    }

    Constant* c = &bytecode->constants[const_index];
    if (c->type != CONST_CLOSURE) {
        vm_runtime_error(vm, "NEW_CLOSURE: expected CONST_CLOSURE");
        return 1;
    }

    uint32_t func_idx = c->closure.func_idx;
    if (func_idx >= bytecode->func_count) {
        vm_runtime_error(vm, "NEW_CLOSURE: bad func index");
        return 1;
    }

    Function* fmeta = &bytecode->functions[func_idx];

    FunctionObject* fn_obj = vm_new_function(
                    vm,
                    fmeta->name ? fmeta->name : "<fn>",
                    fmeta->arity,
                    fmeta->max_locals,
                    fmeta->n_upvalues,
                    bytecode,
                    (int)func_idx);

    ClosureObject* clo = vm_new_closure(vm, fn_obj);

    for (int i = 0; i < fmeta->n_upvalues; i++) {
        UpvalueInfo* uinfo = &fmeta->upvalues[i];
        if (uinfo->is_local) {
            Value* slot = &vm->stack[frame->slots_offset + uinfo->location];
            clo->upvalues[i] = vm_capture_upvalue(vm, slot);
        } else {
            if (!frame->closure ||
            uinfo->location >= frame->closure->upvalue_count) {
                vm_runtime_error(vm, "NEW_CLOSURE: bad upvalue chain");
                return INTERPRET_RUNTIME_ERROR;
            }
            clo->upvalues[i] = frame->closure->upvalues[uinfo->location];
        }
    }

    vm_push(vm, OBJECT_VAL(clo));
    return 0;
}

int vm_op_array_new_u8(VM* vm, CallFrame* frame, uint8_t* ip) {
    uint8_t count = *ip++;
    frame->ip = ip;
    ArrayObject* arr = vm_new_array(vm, count);
    for (int i = count - 1; i >= 0; i--) {
        arr->items[i] = vm_pop(vm);
        arr->count++;
    }
    vm_push(vm, OBJECT_VAL(arr));
    return 0;
}

int vm_op_array_get(VM* vm) {
    if (vm->sp < 2) {
        vm_runtime_error(vm, "ARRAY_GET stack underflow");
        return 1;
    }

    Value index = vm_pop(vm);
    Value array = vm_pop(vm);

    if (!IS_ARRAY(array)) {
        vm_runtime_error(vm, "ARRAY_GET type error (not array)");
        return 1;
    }

    if (!IS_INT(index)) {
        vm_runtime_error(vm, "ARRAY_GET type error (index not int)");
        return 1;
    }

    ArrayObject* arr = AS_ARRAY(array);
    int idx = (int)AS_INT(index);

    if (idx < 0 || idx >= arr->count) {
        vm_runtime_error(vm, "ARRAY_GET out of bounds");
        return 1;
    }

    Value result = arr->items[idx];
    vm_push(vm, result);
    return 0;
}

int vm_op_array_set(VM* vm) {
    Value value = vm_pop(vm);
    Value index = vm_pop(vm);
    Value array = vm_pop(vm);
    if (!IS_ARRAY(array) || !IS_INT(index)) {
        vm_runtime_error(vm, "ARRAY_SET type error");
        return 1;
    }
    ArrayObject* arr = AS_ARRAY(array);
    int idx = (int)AS_INT(index);
    if (idx < 0 || idx >= arr->count) {
        vm_runtime_error(vm, "ARRAY_SET out of bounds");
        return 1;
    }
    arr->items[idx] = value;
    vm_push(vm, value);
    return 0;
}

int vm_op_array_len(VM* vm) {
    Value array = vm_pop(vm);
    if (!IS_ARRAY(array)) {
        vm_runtime_error(vm, "ARRAY_LEN on non-array");
        return 1;
    }
    ArrayObject* arr = AS_ARRAY(array);
    vm_push(vm, INT_VAL(arr->count));
    return 0;
}


int vm_op_load_global_u16(VM* vm, CallFrame* frame, uint8_t* ip) {
    uint16_t idx = read_u16(ip);
    ip += 2;
    frame->ip = ip;
    vm_push(vm, vm_load_global(vm, (int)idx));
    return 0;
}

int vm_op_store_global_u16(VM* vm, CallFrame* frame, uint8_t* ip) {
    uint16_t idx = read_u16(ip);
    ip += 2;
    frame->ip = ip;
    Value v = vm_peek(vm, 0);
    vm_store_global(vm, (int)idx, v);
    return 0;
}

int vm_op_print(VM* vm) {
    Value v = vm_pop(vm);
    vm_print_value(v);
    printf("\n");
    return 0;
}

int64_t jit_pop_and_test_bool(VM* vm) {
    Value v = vm_pop(vm);
    return value_to_bool(&v);
}

int jit_handle_return(VM* vm, CallFrame* frame, uint8_t* ip) {
    Value result = vm_pop(vm);

    vm_pop_frame(vm);

    if (vm->frame_count == 0) {
        vm_push(vm, result);
        return 0;
    }

    vm_push(vm, result);
    frame = &vm->frames[vm->frame_count - 1];
    ip = frame->ip;
    return 0;
}

int jit_handle_return_nil(VM* vm, CallFrame* frame, uint8_t* ip) {
    vm_pop_frame(vm);

    if (vm->frame_count == 0) {
        vm_push(vm, NIL_VAL);
        return 0;
    }

    vm_push(vm, NIL_VAL);
    frame = &vm->frames[vm->frame_count - 1];
    ip = frame->ip;
    return 0;
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
        0);

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

        int op_res = 0;
        switch ((OpCode)opcode) {
            case OP_ADD: {
                op_res = vm_op_add(vm);
                break;
            }
            case OP_SUB: {
                op_res = vm_op_sub(vm);
                break;
            }
            case OP_MUL: {
                op_res = vm_op_mul(vm);
                break;
            }
            case OP_DIV: {
                op_res = vm_op_div(vm);
                break;
            }
            case OP_MOD: {
                op_res = vm_op_mod(vm);
                break;
            }
            case OP_NEG: {
                op_res = vm_op_neg(vm);
                break;
            }
            case OP_NOT: {
                op_res = vm_op_not(vm);
                break;
            }
            case OP_EQ: {
                op_res = vm_op_eq(vm);
                break;
            }
            case OP_NEQ: {
                op_res = vm_op_neq(vm);
                break;
            }
            case OP_LT: {
                op_res = vm_op_lt(vm);
                break;
            }
            case OP_LE: {
                op_res = vm_op_le(vm);
                break;
            }
            case OP_GT: {
                op_res = vm_op_gt(vm);
                break;
            }
            case OP_GE: {
                op_res = vm_op_ge(vm);
                break;
            }
            case OP_AND: {
                op_res = vm_op_and(vm);
                break;
            }
            case OP_OR: {
                op_res = vm_op_or(vm);
                break;
            }
            case OP_NOP:
                break;
            case OP_LOAD_CONST_U16: {
                op_res = vm_op_load_const_u16(vm, bytecode, frame, ip);
                break;
            }
            case OP_LOAD_LOCAL_U8: {
                op_res = vm_op_load_local_u8(vm, frame, ip);
                break;
            }
            case OP_STORE_LOCAL_U8: {
                op_res = vm_op_store_local_u8(vm, frame, ip);
                break;
            }
            case OP_LOAD_GLOBAL_U16: {
                op_res = vm_op_load_global_u16(vm, frame, ip);
                break;
            }
            case OP_STORE_GLOBAL_U16: {
                op_res = vm_op_store_global_u16(vm, frame, ip);
                break;
            }
            case OP_JUMP_U16: {
                op_res = vm_op_jump_u16(vm, frame, ip);
                break;
            }
            case OP_JUMP_IF_FALSE_U16: {
                op_res = vm_op_jump_if_false_u16(vm, frame, ip);
                break;
            }
            case OP_JUMP_IF_TRUE_U16: {
                op_res = vm_op_jump_if_true_u16(vm, frame, ip);
                break;
            }
            case OP_CALL_U8:
                op_res = vm_op_call_u8(vm, frame, ip);
                break;
            case OP_CALL_U16: {
                op_res = vm_op_call_u16(vm, frame, ip);
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
            case OP_RETURN_NIL: {
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
                op_res = vm_op_array_new_u8(vm, frame, ip);
                break;
            }
            case OP_ARRAY_GET: {
                op_res = vm_op_array_get(vm);
                break;
            }
            case OP_ARRAY_SET: {
                op_res = vm_op_array_set(vm);
                break;
            }
            case OP_ARRAY_LEN: {
                op_res = vm_op_array_len(vm);
                break;
            }

            case OP_PRINT: {
                op_res = vm_op_print(vm);
                break;
            }

            case OP_DUP: {
                op_res = vm_op_dup(vm);
                break;
            }

            case OP_POP: {
                (void) vm_pop(vm);
                break;
            }

            case OP_BREAK: {
                break;
            }

            case OP_HALT: {
                return INTERPRET_OK;
            }

            case OP_NEW_CLOSURE: {
                op_res = vm_op_new_closure(vm, bytecode, frame, ip);
                break;
            }

            case OP_LOAD_UPVALUE_U8: {
                op_res = vm_op_load_upvalue_u8(vm, frame, ip);
                break;
            }

            case OP_STORE_UPVALUE_U8: {
                op_res = vm_op_store_upvalue_u8(vm, frame, ip);
                break;
            }
            default:
                vm_runtime_error(vm, "Unknown opcode %d", opcode);
                return INTERPRET_RUNTIME_ERROR;
        }
        if (op_res == 1) return INTERPRET_RUNTIME_ERROR;
    }

    return INTERPRET_OK;
}
