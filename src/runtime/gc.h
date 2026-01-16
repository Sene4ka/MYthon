#ifndef MYTHON_GC_H
#define MYTHON_GC_H

#include "vm.h"

void mark_roots(VM* vm);
void gc_collect_garbage(VM* vm);
void mark_value(VM* vm, Value v);
void mark_object(VM* vm, Object* obj);

void vm_register_bytes(VM* vm, size_t bytes);
void vm_unregister_bytes(VM* vm, size_t bytes);

void* vm_realloc(VM* vm, void* ptr, size_t old_size, size_t new_size);
void gc_check(VM* vm);
void link_object(VM* vm, Object* obj, uint8_t type);

#endif
