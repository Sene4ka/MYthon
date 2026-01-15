#ifndef MYTHON_MEMORY_H
#define MYTHON_MEMORY_H

#include <stddef.h>

void* allocate(size_t size);
void* reallocate(void* ptr, size_t new_size);
void free_ptr(void* ptr);

#define ALLOCATE(type, count) \
(type*)allocate(sizeof(type) * (count))

#define GROW_ARRAY(type, pointer, old_count, new_count) \
(type*)reallocate(pointer, sizeof(type) * (new_count))

#define FREE_ARRAY(type, pointer, old_count) \
free_ptr(pointer)

#define GROW_CAPACITY(capacity) \
((capacity) < 8 ? 8 : (capacity) * 2)

#endif
