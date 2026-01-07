#ifndef MYTHON_GC_H
#define MYTHON_GC_H

#include "vm.h"

void vm_mark_roots(VM* vm);
void vm_collect_garbage(VM* vm);
void mark_value(VM* vm, Value v);
void mark_object(VM* vm, Object* obj);

void* vm_realloc(VM* vm, void* ptr, size_t old_size, size_t new_size);
void link_object(VM* vm, Object* obj, uint8_t type);

#endif
