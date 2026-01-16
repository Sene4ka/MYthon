#ifndef MYTHON_UTILS_H
#define MYTHON_UTILS_H

#include <stddef.h>

typedef struct {
    unsigned char* data;
    size_t length;
    size_t capacity;
} ByteBuffer;

ByteBuffer* read_file_to_buffer(const char* filename);
ByteBuffer* create_buffer(size_t initial_capacity);
void append_to_buffer(ByteBuffer* buffer, const unsigned char* data, size_t length);
void free_buffer(ByteBuffer* buffer);
char* read_entire_file(const char* filename, size_t* out_length);
int is_binary_file(const unsigned char* data, size_t length);
void print_buffer_hex(const ByteBuffer* buffer, size_t max_bytes);
char* copy_string(const char* source);
char* format_string(const char* format, ...);

#endif
