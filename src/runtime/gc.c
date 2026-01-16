#include "gc.h"

#include "utils/memory.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void vm_register_bytes(VM* vm, size_t bytes) {
    vm->bytes_allocated += bytes;
}

void vm_unregister_bytes(VM* vm, size_t bytes) {
    vm->bytes_allocated += bytes;
}

void* vm_realloc(VM* vm, void* ptr, size_t old_size, size_t new_size) {
    if (new_size > old_size) {
        vm_register_bytes(vm, new_size - old_size);
    } else if (new_size < old_size) {
        vm_unregister_bytes(vm, new_size - old_size);
    }

    if (new_size == 0) {
        free_ptr(ptr);
        return NULL;
    }

    void* new_ptr = reallocate(ptr, new_size);

    return new_ptr;
}

void gc_check(VM* vm) {
    if (vm->gc_enabled && !vm->gc_collecting && vm->bytes_allocated > vm->next_gc) {
        gc_collect_garbage(vm);
    }
}

void link_object(VM* vm, Object* obj, uint8_t type) {
    obj->type = type;
    obj->marked = 1; // gc safe for 1 turn
    obj->next = vm->objects;
    vm->objects = obj;
}

void mark_value(VM* vm, Value v) {
    if (!IS_OBJECT(v)) return;

    Object* obj = AS_OBJECT(v);
    if (obj == NULL || (uintptr_t)obj < 0x10000) {
        if (vm->debug_gc) {
            printf( "[GC-WARN] Skipping invalid Value{type=%d, obj=%p}\n",
                    v.type, (void*)obj);
        }
        return;
    }

    mark_object(vm, obj);
}


void mark_object(VM* vm, Object* obj) {
    if (obj == NULL || (uintptr_t)obj < 0x10000) {
        if (vm->debug_gc) {
            printf( "[GC-WARN] Invalid object pointer: %p\n", (void*)obj);
        }
        return;
    }

    if (obj->marked) return;
    obj->marked = 1;

    switch (obj->type) {
        case VAL_STRING: {
            break;
        }
        case VAL_ARRAY: {
            ArrayObject* arr = (ArrayObject*)obj;
            for (int i = 0; i < arr->count; i++) {
                mark_value(vm, arr->items[i]);
            }
            break;
        }
        case VAL_FUNCTION: {
            FunctionObject* fn = (FunctionObject*)obj;
            (void)fn;
            break;
        }
        case VAL_NATIVE_FN: {
            break;
        }
        case VAL_CLOSURE: {
            ClosureObject* cl = (ClosureObject*)obj;
            mark_object(vm, (Object*)cl->function);
            for (int i = 0; i < cl->upvalue_count; i++) {
                Upvalue* uv = cl->upvalues[i];
                if (!uv) continue;
                mark_object(vm, (Object*)uv);
            }
            break;
        }
        case VAL_UPVALUE: {
            Upvalue* uv = (Upvalue*)obj;
            if (uv->location == NULL || uv->location == &uv->closed) {
                mark_value(vm, uv->closed);
            } else {
                mark_value(vm, *uv->location);
            }
            break;
        }
        default:
            break;
    }
}

void mark_roots(VM* vm) {
    if (!vm || !vm->stack) return;

    for (int f = 0; f < vm->frame_count; f++) {
        CallFrame* frame = &vm->frames[f];
        int frame_start = frame->slots_offset;
        int frame_end = frame_start + frame->slot_count;

        if (frame_end > vm->sp) {
            frame_end = vm->sp;
        }

        for (int i = frame_start; i < frame_end; i++) {
            Value val = vm->stack[i];
            mark_value(vm, val);
        }
    }

    if (vm->frame_count > 0) {
        CallFrame* top_frame = &vm->frames[vm->frame_count - 1];
        int top_frame_end = top_frame->slots_offset + top_frame->slot_count;

        if (top_frame_end < vm->sp) {
            for (int i = top_frame_end; i < vm->sp; i++) {
                Value val = vm->stack[i];
                mark_value(vm, val);
            }
        }
    } else if (vm->sp > 0) {
        for (int i = 0; i < vm->sp; i++) {
            Value val = vm->stack[i];
            mark_value(vm, val);
        }
    }

    for (int i = 0; i < vm->global_count; i++) {
        Value val = vm->globals[i];

        mark_value(vm, val);
    }

    for (int i = 0; i < vm->constants.count; i++) {
        mark_value(vm, vm->constants.values[i]);
    }

    for (int i = 0; i < vm->frame_count; i++) {
        CallFrame* f = &vm->frames[i];
        if (f->closure) {
            mark_object(vm, (Object*)f->closure);
        }
    }

    for (Upvalue* uv = vm->open_upvalues; uv != NULL; uv = uv->next) {
        mark_object(vm, (Object*)uv);

        if (uv->location && uv->location != &uv->closed) {
            mark_value(vm, *uv->location);
        }
        mark_value(vm, uv->closed);
    }
}

void gc_collect_garbage(VM* vm) {
    if (vm->gc_collecting) {
        if (vm->debug_gc) {
            printf( "[GC] Already collecting, skipping recursive call\n");
        }
        return;
    }
    
    vm->gc_collecting = 1;
    
    size_t before = vm->bytes_allocated;
    int freed_objects = 0;

    mark_roots(vm);

    Object* prev = NULL;
    Object* obj = vm->objects;

    while (obj != NULL) {
        if (!obj->marked) {
            Object* unreached = obj;
            obj = obj->next;

            if (prev) prev->next = obj;
            else vm->objects = obj;

            switch (unreached->type) {
                case VAL_STRING: {
                    StringObject* s = (StringObject*)unreached;
                    if (s->chars) {
                        vm_realloc(vm, s->chars,
                                   (size_t)s->length + 1, 0);
                    }
                    vm_realloc(vm, s, sizeof(StringObject), 0);
                    break;
                }
                case VAL_ARRAY: {
                    ArrayObject* a = (ArrayObject*)unreached;
                    if (a->items && a->capacity > 0) {
                        vm_realloc(vm, a->items,
                                   sizeof(Value) * (size_t)a->capacity, 0);
                    }
                    vm_realloc(vm, a, sizeof(ArrayObject), 0);
                    break;
                }
                case VAL_FUNCTION: {
                    FunctionObject* fn = (FunctionObject*)unreached;
                    if (fn->name) {
                        vm_realloc(vm, fn->name,
                                   strlen(fn->name) + 1, 0);
                    }
                    vm_realloc(vm, fn, sizeof(FunctionObject), 0);
                    break;
                }
                case VAL_NATIVE_FN: {
                    NativeFunctionObject* nf =
                        (NativeFunctionObject*)unreached;
                    if (nf->name) {
                        vm_realloc(vm, nf->name,
                                   strlen(nf->name) + 1, 0);
                    }
                    vm_realloc(vm, nf, sizeof(NativeFunctionObject), 0);
                    break;
                }
                case VAL_CLOSURE: {
                    ClosureObject* cl = (ClosureObject*)unreached;
                    if (cl->upvalues && cl->upvalue_count > 0) {
                        vm_realloc(vm, cl->upvalues,
                                   sizeof(Upvalue*) *
                                       (size_t)cl->upvalue_count,
                                   0);
                    }
                    vm_realloc(vm, cl, sizeof(ClosureObject), 0);
                    break;
                }
                case VAL_UPVALUE: {
                    Upvalue* uv = (Upvalue*)unreached;
                    vm_realloc(vm, uv, sizeof(Upvalue), 0);
                    break;
                }
                default:
                    break;
            }

            freed_objects++;
        } else {
            if (obj){
                obj->marked = 0;
                prev = obj;
                obj = obj->next;
            }
        }
    }

    size_t after = vm->bytes_allocated;
    vm->next_gc = vm->bytes_allocated + vm->bytes_allocated / 2;

    if (vm->debug_gc) {
        printf(
                "[GC] collected %d objects, freed %zu bytes (now %zu bytes, next_gc=%zu)\n",
                freed_objects,
                before > after ? before - after : 0,
                after,
                vm->next_gc);
    }
    
    vm->gc_collecting = 0;
}

