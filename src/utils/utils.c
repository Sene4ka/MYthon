#include "utils.h"
#include "memory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

ByteBuffer* create_buffer(size_t initial_capacity) {
    ByteBuffer* buffer = ALLOCATE(ByteBuffer, 1);
    buffer->capacity = initial_capacity > 0 ? initial_capacity : 64;
    buffer->length = 0;
    buffer->data = ALLOCATE(unsigned char, buffer->capacity);
    return buffer;
}

void append_to_buffer(ByteBuffer* buffer, const unsigned char* data, size_t length) {
    if (buffer->length + length > buffer->capacity) {
        size_t new_capacity = buffer->capacity;
        while (buffer->length + length > new_capacity) {
            new_capacity *= 2;
        }
        buffer->data = GROW_ARRAY(unsigned char, buffer->data,
                                  buffer->capacity, new_capacity);
        buffer->capacity = new_capacity;
    }
    memcpy(buffer->data + buffer->length, data, length);
    buffer->length += length;
}

void free_buffer(ByteBuffer* buffer) {
    if (buffer) {
        FREE_ARRAY(unsigned char, buffer->data, buffer->capacity);
        FREE_ARRAY(ByteBuffer, buffer, 1);
    }
}

ByteBuffer* read_file_to_buffer(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Не удалось открыть файл: %s\n", filename);
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (file_size < 0) {
        fclose(file);
        fprintf(stderr, "Ошибка определения размера файла: %s\n", filename);
        return NULL;
    }
    ByteBuffer* buffer = create_buffer(file_size + 1);
    unsigned char* temp = ALLOCATE(unsigned char, file_size);
    size_t bytes_read = fread(temp, 1, file_size, file);
    if (bytes_read != (size_t)file_size) {
        fprintf(stderr, "Ошибка чтения файла: %s\n", filename);
        free_buffer(buffer);
        FREE_ARRAY(unsigned char, temp, file_size);
        fclose(file);
        return NULL;
    }
    append_to_buffer(buffer, temp, bytes_read);
    unsigned char null_byte = '\0';
    append_to_buffer(buffer, &null_byte, 1);
    buffer->length--;
    FREE_ARRAY(unsigned char, temp, file_size);
    fclose(file);
    return buffer;
}

int is_binary_file(const unsigned char* data, size_t length) {
    size_t check_len = length < 1024 ? length : 1024;
    for (size_t i = 0; i < check_len; i++) {
        if (data[i] == 0) {
            return 1;
        }
    }
    return 0;
}

void print_buffer_hex(const ByteBuffer* buffer, size_t max_bytes) {
    size_t bytes_to_print = buffer->length;
    if (max_bytes > 0 && bytes_to_print > max_bytes) {
        bytes_to_print = max_bytes;
    }
    printf("Буфер (%zu/%zu байт):\n", bytes_to_print, buffer->length);
    for (size_t i = 0; i < bytes_to_print; i++) {
        printf("%02x ", buffer->data[i]);
        if ((i + 1) % 16 == 0) printf("\n");
    }
    printf("\n");
}

char* copy_string(const char* source) {
    size_t length = strlen(source);
    char* copy = ALLOCATE(char, length + 1);
    memcpy(copy, source, length);
    copy[length] = '\0';
    return copy;
}

char* format_string(const char* format, ...) {
    va_list args;
    va_start(args, format);
    va_list args_copy;
    va_copy(args_copy, args);
    int size = vsnprintf(NULL, 0, format, args_copy) + 1;
    va_end(args_copy);
    if (size <= 0) {
        va_end(args);
        return copy_string("");
    }
    char* result = ALLOCATE(char, size);
    vsnprintf(result, size, format, args);
    va_end(args);
    return result;
}