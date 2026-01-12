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

static void vm_gc_disable(VM* vm) {
    vm->gc_enabled = 0;
}

static void vm_gc_enable(VM* vm) {
    vm->gc_enabled = 1;
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

        case OP_NEW_CLOSURE: {
            uint16_t idx = (uint16_t)(ip[1] << 8 | ip[2]);
            fprintf(stderr, " %u", idx);
            break;
        }

        default:
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

    //vm_gc_disable(vm);

    FREE_ARRAY(Value, vm->stack, vm->stack_capacity);
    FREE_ARRAY(CallFrame, vm->frames, vm->frame_capacity);
    FREE_ARRAY(Value, vm->globals, vm->global_capacity);
    FREE_ARRAY(Value, vm->constants.values, vm->constants.capacity);

    //vm_collect_garbage(vm);

    jit_vm_cleanup(vm);

    //vm_gc_enable(vm);

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


StringObject* vm_take_string(VM* vm, char* chars, int length) {
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] new StringObject len=%d\n", length);
    }

    StringObject* str = (StringObject*)vm_realloc(
            vm, NULL, 0, sizeof(StringObject));

    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] StringObject ptr=%p\n", (void*)str);
    }

    link_object(vm, (Object*)str, VAL_STRING);

    str->chars = chars;
    str->length = length;
    str->hash = hash_string(chars, length);
    return str;
}

StringObject* vm_copy_string(VM* vm, const char* chars, int length) {
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] copy_string len=%d\n", length);
    }

    char* copy = (char*)vm_realloc(vm, NULL, 0, (size_t)length + 1);

    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] copy_string buffer=%p\n", (void*)copy);
    }

    memcpy(copy, chars, (size_t)length);
    copy[length] = '\0';
    return vm_take_string(vm, copy, length);
}

ArrayObject* vm_new_array(VM* vm, int capacity) {

    vm_gc_disable(vm);

    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] new ArrayObject cap=%d\n", capacity);
    }

    ArrayObject* arr = (ArrayObject*)vm_realloc(
            vm, NULL, 0, sizeof(ArrayObject));

    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] ArrayObject ptr=%p\n", (void*)arr);
    }

    link_object(vm, (Object*)arr, VAL_ARRAY);

    if (vm->debug_gc) fprintf(stderr, "[GC-OBJ] ArrayObject linked\n");

    arr->capacity = capacity > 0 ? capacity : 0;
    arr->count = 0;

    if (vm->debug_gc) fprintf(stderr, "[GC-OBJ] ArrayObject init cap=%d count=%d\n ", arr->capacity, arr->count);

    if (arr->capacity > 0) {
        if (vm->debug_gc) {
            fprintf(stderr, "[GC-OBJ]   items alloc cap=%d size=%zu\n",
                    arr->capacity, sizeof(Value) * (size_t)arr->capacity);
        }

        arr->items = (Value*)vm_realloc(
                vm, NULL, 0, sizeof(Value) * (size_t)arr->capacity);

        if (vm->debug_gc) {
            fprintf(stderr, "[GC-OBJ]   items ptr=%p\n", (void*)arr->items);
        }
    } else {
        if (vm->debug_gc) fprintf(stderr, "[GC-OBJ] ArrayObject cap 0");
        arr->items = NULL;
    }
    if (vm->debug_gc) fprintf(stderr, "[GC-OBJ] ArrayObject created");

    vm_gc_enable(vm);

    return arr;
}

void vm_array_append(VM* vm, ArrayObject* array, Value value) {
    if (!array) return;

    if (array->count >= array->capacity) {
        int old_cap = array->capacity;
        int new_cap = old_cap < 8 ? 8 : old_cap * 2;

        if (vm->debug_gc) {
            fprintf(stderr,
                    "[GC-OBJ] array grow old=%d new=%d ptr=%p\n",
                    old_cap, new_cap, (void*)array->items);
        }

        array->items = (Value*)vm_realloc(
                vm,
                array->items,
                sizeof(Value) * (size_t)old_cap,
                sizeof(Value) * (size_t)new_cap
        );

        if (vm->debug_gc) {
            fprintf(stderr,
                    "[GC-OBJ] array grown ptr=%p\n", (void*)array->items);
        }

        array->capacity = new_cap;
    }

    array->items[array->count++] = value;
}

NativeFunctionObject* vm_new_native_function(VM* vm, const char* name, NativeFn function, int arity) {

    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] new NativeFn name=%s\n", name);
    }

    vm_gc_disable(vm);

    NativeFunctionObject* nf = (NativeFunctionObject*)vm_realloc(
            vm, NULL, 0, sizeof(NativeFunctionObject));

    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] NativeFn ptr=%p\n", (void*)nf);
    }

    link_object(vm, (Object*)nf, VAL_NATIVE_FN);

    nf->function = function;
    nf->arity = arity;

    int len = (int)strlen(name);
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ]   name len=%d\n", len);
    }

    nf->name = (char*)vm_realloc(vm, NULL, 0, (size_t)len + 1);

    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ]   name ptr=%p\n", (void*)nf->name);
    }

    memcpy(nf->name, name, (size_t)len + 1);

    vm_gc_enable(vm);

    return nf;
}

FunctionObject* vm_new_function(VM* vm,
                               const char* name,
                               int arity,
                               int local_count,
                               int upvalue_count,
                               Bytecode* bytecode,
                               int func_index) {
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] new Function name=%s\n", name ? name : "");
    }

    vm_gc_disable(vm);

    FunctionObject* fn = (FunctionObject*)vm_realloc(
        vm, NULL, 0, sizeof(FunctionObject));
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] Function ptr=%p\n", (void*)fn);
    }

    link_object(vm, (Object*)fn, VAL_FUNCTION);

    int len = (int)strlen(name);
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] name len=%d\n", len);
    }

    fn->name = (char*)vm_realloc(vm, NULL, 0, (size_t)len + 1);
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] name ptr=%p\n", (void*)fn->name);
    }

    memcpy(fn->name, name, (size_t)len + 1);
    fn->arity = arity;
    fn->local_count = local_count;
    fn->upvalue_count = upvalue_count;
    fn->bytecode = bytecode;
    fn->func_index = func_index;

    vm_gc_enable(vm);

    return fn;
}


ClosureObject* vm_new_closure(VM* vm, FunctionObject* function) {
    uint8_t upvalue_count = function->upvalue_count;
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] new Closure func=%s upvalues=%d\n",
                function && function->name ? function->name : "",
                upvalue_count);
    }

    vm_gc_disable(vm);

    ClosureObject* cl = (ClosureObject*)vm_realloc(
        vm, NULL, 0, sizeof(ClosureObject));
    if (vm->debug_gc) {
        fprintf(stderr, "[GC-OBJ] Closure ptr=%p\n", (void*)cl);
    }

    link_object(vm, (Object*)cl, VAL_CLOSURE);
    cl->function = function;
    cl->upvalue_count = upvalue_count;

    if (cl->upvalue_count > 0) {
        if (vm->debug_gc) {
            fprintf(stderr, "[GC-OBJ] upvalues count=%d\n",
                    cl->upvalue_count);
        }

        cl->upvalues = (Upvalue**)vm_realloc(
            vm, NULL, 0, sizeof(Upvalue*) * (size_t)cl->upvalue_count);
        if (vm->debug_gc) {
            fprintf(stderr, "[GC-OBJ] upvalues ptr=%p\n",
                    (void*)cl->upvalues);
        }

        for (int i = 0; i < cl->upvalue_count; i++) {
            cl->upvalues[i] = NULL;
        }
    } else {
        cl->upvalues = NULL;
    }

    vm_gc_enable(vm);

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

Value vm_call_value(VM* vm, Value callee, int arg_count) {
    if (IS_NATIVE(callee)) {
        NativeFunctionObject* nf = AS_NATIVE(callee);
        return vm_call_native(vm, nf->function, arg_count);
    }

    if (IS_CLOSURE(callee)) {
        ClosureObject* cl = AS_CLOSURE(callee);
        FunctionObject* fn = cl->function;
        if (!fn->bytecode) {
            vm_runtime_error(vm, "Closure has no bytecode");
            return NIL_VAL;
        }

        int callee_index = vm->sp - 1 - arg_count;

        vm_push_frame_with_ip(
            vm,
            fn->bytecode,
            fn->local_count,
            fn->bytecode->code + fn->bytecode->functions[fn->func_index].code_start
        );
        CallFrame* new_frame = &vm->frames[vm->frame_count - 1];
        new_frame->closure = cl;

        for (int i = 0; i < arg_count; i++) {
            vm_store_local(vm, i, vm->stack[callee_index + 1 + i]);
        }

        vm->sp = callee_index;

        return NIL_VAL;
    }

    vm_runtime_error(vm, "Attempt to call non-callable value");
    return NIL_VAL;
}

static Upvalue* vm_capture_upvalue(VM* vm, Value* slot) {
    Upvalue* prev = NULL;
    Upvalue* uv = vm->open_upvalues;

    vm_gc_disable(vm);

    while (uv && uv->location > slot) {
        prev = uv;
        uv = uv->next;
    }

    if (uv && uv->location == slot) {
        vm_gc_enable(vm);
        return uv;
    }

    Upvalue* created = (Upvalue*)vm_realloc(vm, NULL, 0, sizeof(Upvalue));
    link_object(vm, (Object*)created, VAL_UPVALUE);

    created->location = slot;
    created->closed = NIL_VAL;
    created->next = uv;

    if (prev) prev->next = created;
    else vm->open_upvalues = created;

    vm_gc_enable(vm);
    return created;
}

static void vm_close_upvalues(VM* vm, Value* last) {
    vm_gc_disable(vm);

    while (vm->open_upvalues && vm->open_upvalues->location >= last) {
        Upvalue* uv = vm->open_upvalues;
        uv->closed = *uv->location;
        uv->location = &uv->closed;
        vm->open_upvalues = uv->next;
    }

    vm_gc_enable(vm);
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
                    printf("[DEBUG SUB] ip=%u\n", *vm->frames[vm->frame_count - 1].ip);
                    printf("[DEBUG SUB] a=");
                    vm_print_value(a);
                    printf(" b=");
                    vm_print_value(b);
                    printf("\n");

                    printf("[DEBUG SUB] a_type=%d(%s) b_type=%d(%s)\n",
                       a.type, vm_value_type_name(a),
                       b.type, vm_value_type_name(b));

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

                if (!((IS_INT(a) || IS_FLOAT(a)) &&
                      (IS_INT(b) || IS_FLOAT(b)))) {
                    vm_runtime_error(vm, "DIV on non-numbers");
                    return INTERPRET_RUNTIME_ERROR;
                      }

                if (IS_INT(a) && IS_INT(b)) {
                    int64_t ia = AS_INT(a);
                    int64_t ib = AS_INT(b);
                    if (ib == 0) {
                        vm_runtime_error(vm, "Division by zero");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    vm_push(vm, INT_VAL(ia / ib));
                } else {
                    double da = IS_INT(a) ? (double)AS_INT(a) : AS_FLOAT(a);
                    double db = IS_INT(b) ? (double)AS_INT(b) : AS_FLOAT(b);
                    if (db == 0.0) {
                        vm_runtime_error(vm, "Division by zero");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    vm_push(vm, FLOAT_VAL(da / db));
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
                        v = INT_VAL((int64_t)idx);
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
                Value cond = vm_pop(vm);
                if (!value_to_bool(cond)) {
                    ip += offset;
                }
                frame->ip = ip;
                break;
            }
            case OP_JUMP_IF_TRUE_U16: {
                int16_t offset = (int16_t)read_u16(ip);
                ip += 2;
                Value cond = vm_pop(vm);
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

                if (vm->debug) {
                    fprintf(stderr,
                            "[CALL] opcode=%s argc=%d sp=%d\n",
                            opcode == OP_CALL_U8 ? "CALL_U8" : "CALL_U16",
                            argc, vm->sp);
                }

                if (vm->sp < 1 + argc) {
                    vm_runtime_error(vm, "CALL: stack underflow (argc=%d, sp=%d)", argc, vm->sp);
                    return INTERPRET_RUNTIME_ERROR;
                }

                int callee_index = vm->sp - 1 - argc;
                Value callee = vm->stack[callee_index];

                if (IS_CLOSURE(callee)) {
                    ClosureObject* cl = AS_CLOSURE(callee);
                    uint32_t func_idx = (uint32_t)cl->function->func_index;

                    bool handled = jit_vm_handle_call(vm, func_idx, frame->bytecode);
                    if (handled) {
                        frame = &vm->frames[vm->frame_count - 1];
                        ip = frame->ip;
                        break;
                    }
                }

                if (vm->debug) {
                    fprintf(stderr,
                            "[CALL] callee_index=%d type=[%s]\n",
                            callee_index, vm_value_type_name(callee));
                }

                if (IS_NATIVE(callee)) {
                    NativeFunctionObject* nf = AS_NATIVE(callee);
                    Value* args = argc > 0 ? ALLOCATE(Value, argc) : NULL;

                    if (vm->debug) {
                        fprintf(stderr, "[CALL] allocate native done\n");
                    }
                    for (int i = 0; i < argc; i++) {
                        args[i] = vm->stack[callee_index + 1 + i];
                    }
                    if (vm->debug) {
                        fprintf(stderr, "[CALL] native args loaded\n");
                    }
                    Value result = nf->function(vm, argc, args);
                    if (vm->debug) {
                        fprintf(stderr, "[CALL] native call result done\n");
                    }
                    if (argc > 0) FREE_ARRAY(Value, args, argc);
                    if (vm->debug) {
                        fprintf(stderr, "[CALL] native free done\n");
                    }
                    vm->sp = callee_index;
                    vm_push(vm, result);
                    if (vm->debug) {
                        fprintf(stderr, "[CALL] native done, new sp=%d\n", vm->sp);
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
                        fprintf(stderr,
                                "[CALL] closure func=%s argc=%d slots_offset=%d\n",
                                fn->name ? fn->name : "<fn>",
                                argc, slots_offset);
                    }

                    for (int i = 0; i < argc; i++) {
                        vm->stack[slots_offset + i] = vm->stack[callee_index + 1 + i];
                    }

                    vm->sp = slots_offset + argc;

                    vm_push_frame_with_ip_at(
                        vm,
                        fn->bytecode,
                        fn->local_count,
                        fn->bytecode->code + fn->bytecode->functions[fn->func_index].code_start,
                        slots_offset
                    );

                    CallFrame* new_frame = &vm->frames[vm->frame_count - 1];
                    new_frame->closure = cl;

                    frame = new_frame;
                    ip = frame->ip;

                    if (vm->debug) {
                        fprintf(stderr,
                                "[CALL] new frame: func=%s slots_offset=%d slot_count=%d sp=%d\n",
                                fn->name ? fn->name : "<fn>",
                                frame->slots_offset, frame->slot_count, vm->sp);
                    }
                } else {
                    vm_runtime_error(vm, "Attempt to call non-callable value");
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (vm->debug) {
                    fprintf(stderr, "[DEBUG] CALL end\n");
                }
                break;
            }


            case OP_RETURN: {
                Value result = vm_pop(vm);
                CallFrame* old_frame = &vm->frames[vm->frame_count - 1];

                vm_close_upvalues(vm, &vm->stack[frame->slots_offset]);

                vm->sp = old_frame->slots_offset;

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
                CallFrame* old_frame = &vm->frames[vm->frame_count - 1];

                vm_close_upvalues(vm, &vm->stack[frame->slots_offset]);

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

                Value constVal = vm_pop(vm);
                if (!IS_INT(constVal)) {
                    vm_runtime_error(vm, "NEW_CLOSURE: expected const index on stack");
                    return INTERPRET_RUNTIME_ERROR;
                }

                int64_t raw = AS_INT(constVal);
                if (raw < 0) {
                    vm_runtime_error(vm, "NEW_CLOSURE: negative const index");
                    return INTERPRET_RUNTIME_ERROR;
                }

                size_t const_index = (size_t)raw;
                if (const_index >= bytecode->const_count) {
                    vm_runtime_error(vm, "NEW_CLOSURE: bad const index");
                    return INTERPRET_RUNTIME_ERROR;
                }

                Constant* c = &bytecode->constants[const_index];
                if (c->type != CONST_CLOSURE) {
                    vm_runtime_error(vm, "NEW_CLOSURE: expected CONST_CLOSURE");
                    return INTERPRET_RUNTIME_ERROR;
                }

                uint32_t func_idx = c->closure.func_idx;
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
                if (!uv) {
                    vm_runtime_error(vm, "LOAD_UPVALUE: null upvalue");
                    return INTERPRET_RUNTIME_ERROR;
                }
                Value val = (uv->location != NULL) ? *uv->location : uv->closed;
                vm_push(vm, val);
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

Value* vm_get_global(VM* vm, uint16_t index) {
    if (index >= vm->global_count) {
        vm_runtime_error(vm, "Global variable index out of bounds");
        static Value nil = NIL_VAL;
        return &nil;
    }
    return &vm->globals[index];
}

void vm_set_global(VM* vm, uint16_t index, Value* value) {
    if (index >= vm->global_count) {
        vm_runtime_error(vm, "Global variable index out of bounds");
        return;
    }
    vm->globals[index] = *value;
}

int vm_call_function(VM* vm, int arg_count) {
    int callee_index = vm->sp - 1 - arg_count;
    Value callee = vm->stack[callee_index];

    if (!IS_CLOSURE(callee)) {
        vm_runtime_error(vm, "Can only call closures");
        return 1;
    }

    ClosureObject* cl = AS_CLOSURE(callee);
    FunctionObject* fn = cl->function;

    if (arg_count != fn->arity) {
        vm_runtime_error(vm, "Expected %d arguments but got %d", fn->arity, arg_count);
        return 1;
    }

    if (vm->frame_count >= vm->frame_capacity) {
        vm_runtime_error(vm, "Stack overflow");
        return 1;
    }

    int slots_offset = callee_index + 1;

    for (int i = 0; i < arg_count; i++) {
        vm->stack[slots_offset + i] = vm->stack[callee_index + 1 + i];
    }

    vm->sp = slots_offset + arg_count;

    vm_push_frame_with_ip_at(vm, fn->bytecode, fn->local_count,
                             fn->bytecode->code + fn->bytecode->functions[fn->func_index].code_start,
                             slots_offset);

    CallFrame* new_frame = &vm->frames[vm->frame_count - 1];
    new_frame->closure = cl;

    return 0;
}

void vm_print(VM* vm) {
    if (vm->sp <= 0) return;
    Value val = vm->stack[--vm->sp];
    vm_print_value(val);
    printf("\n");
}

int vm_array_get(VM* vm) {
    if (vm->sp < 2) {
        vm_runtime_error(vm, "Stack underflow in array_get");
        return 1;
    }

    Value index_val = vm->stack[--vm->sp];
    Value array_val = vm->stack[--vm->sp];

    if (!IS_ARRAY(array_val)) {
        vm_runtime_error(vm, "Array get on non-array");
        return 1;
    }

    if (!IS_INT(index_val)) {
        vm_runtime_error(vm, "Array index must be integer");
        return 1;
    }

    ArrayObject* arr = AS_ARRAY(array_val);
    int idx = (int)AS_INT(index_val);

    if (idx < 0 || idx >= arr->count) {
        vm_runtime_error(vm, "Array index out of bounds");
        return 1;
    }

    vm->stack[vm->sp++] = arr->items[idx];
    return 0;
}

int vm_array_set(VM* vm) {
    if (vm->sp < 3) {
        vm_runtime_error(vm, "Stack underflow in array_set");
        return 1;
    }

    Value value = vm->stack[--vm->sp];
    Value index_val = vm->stack[--vm->sp];
    Value array_val = vm->stack[--vm->sp];

    if (!IS_ARRAY(array_val)) {
        vm_runtime_error(vm, "Array set on non-array");
        return 1;
    }

    if (!IS_INT(index_val)) {
        vm_runtime_error(vm, "Array index must be integer");
        return 1;
    }

    ArrayObject* arr = AS_ARRAY(array_val);
    int idx = (int)AS_INT(index_val);

    if (idx < 0 || idx >= arr->count) {
        vm_runtime_error(vm, "Array index out of bounds");
        return 1;
    }

    arr->items[idx] = value;
    vm->stack[vm->sp++] = value;
    return 0;
}

int vm_array_len(VM* vm) {
    if (vm->sp < 1) {
        vm_runtime_error(vm, "Stack underflow in array_len");
        return 1;
    }

    Value array_val = vm->stack[--vm->sp];

    if (!IS_ARRAY(array_val)) {
        vm_runtime_error(vm, "Array len on non-array");
        return 1;
    }

    ArrayObject* arr = AS_ARRAY(array_val);
    vm->stack[vm->sp++] = INT_VAL(arr->count);

    return 0;
}
