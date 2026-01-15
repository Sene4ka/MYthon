#include "memory.h"
#include <stdlib.h>
#include <stdio.h>

#ifdef DEBUG_MEMORY
static size_t total_allocated = 0;
static size_t total_freed = 0;
#endif

void* allocate(size_t size) {
    void* ptr = calloc(1, size);

    if (ptr == NULL && size > 0) {
        fprintf(stderr, "Ошибка выделения памяти: %zu байт\n", size);
        exit(1);
    }

#ifdef DEBUG_MEMORY
    total_allocated += size;
    printf("[MEM] alloc %zu -> %p (total: %zu)\n",
           size, ptr, total_allocated - total_freed);
#endif

    return ptr;
}

void* reallocate(void* ptr, size_t new_size) {
    void* new_ptr = realloc(ptr, new_size);

    if (new_ptr == NULL && new_size > 0) {
        fprintf(stderr, "Ошибка перевыделения памяти: %zu байт\n", new_size);
        exit(1);
    }

#ifdef DEBUG_MEMORY
    printf("[MEM] realloc %p -> %p (%zu bytes)\n", ptr, new_ptr, new_size);
#endif

    return new_ptr;
}

void free_ptr(void* ptr) {
    if (ptr != NULL) {
#ifdef DEBUG_MEMORY
        printf("[MEM] free %p\n", ptr);
        total_freed += 0;
#endif
        free(ptr);
    }
}

#ifdef DEBUG_MEMORY
void memory_report() {
    printf("[MEM] Отчёт:\n");
    printf("  Выделено: %zu байт\n", total_allocated);
    printf("  Освобождено: %zu байт\n", total_freed);
    printf("  Утечка: %zu байт\n", total_allocated - total_freed);
}
#endif
