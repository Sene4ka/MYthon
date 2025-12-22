#include "utils.h"
#include "memory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

ByteBuffer* read_file_to_buffer(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    fseek(file, 0, SEEK_SET);

    ByteBuffer* buffer = create_buffer(length);
    if (!buffer) {
        fclose(file);
        return NULL;
    }

    size_t read = fread(buffer->data, 1, length, file);
    buffer->length = read;

    fclose(file);
    return buffer;
}

ByteBuffer* create_buffer(size_t initial_capacity) {
    ByteBuffer* buffer = ALLOCATE(ByteBuffer, 1);
    if (!buffer) {
        return NULL;
    }

    buffer->capacity = initial_capacity > 0 ? initial_capacity : 256;
    buffer->data = ALLOCATE(unsigned char, buffer->capacity);
    buffer->length = 0;

    if (!buffer->data) {
        free_ptr(buffer);
        return NULL;
    }

    return buffer;
}

void append_to_buffer(ByteBuffer* buffer, const unsigned char* data, size_t length) {
    if (buffer->length + length > buffer->capacity) {
        size_t new_capacity = buffer->capacity;
        while (new_capacity < buffer->length + length) {
            new_capacity *= 2;
        }

        unsigned char* new_data = GROW_ARRAY(unsigned char, buffer->data, buffer->capacity, new_capacity);
        if (!new_data) {
            return;
        }

        buffer->data = new_data;
        buffer->capacity = new_capacity;
    }

    memcpy(buffer->data + buffer->length, data, length);
    buffer->length += length;
}

void free_buffer(ByteBuffer* buffer) {
    if (buffer) {
        FREE_ARRAY(unsigned char, buffer->data, buffer->capacity);
        free_ptr(buffer);
    }
}

char* read_entire_file(const char* filename, size_t* out_length) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (length <= 0) {
        fclose(file);
        *out_length = 0;
        char* result = ALLOCATE(char, 1);
        result[0] = '\0';
        return result;
    }

    char* content = ALLOCATE(char, length + 1);
    if (!content) {
        fclose(file);
        return NULL;
    }

    size_t read = fread(content, 1, length, file);
    content[read] = '\0';

    fclose(file);
    *out_length = read;
    return content;
}

int is_binary_file(const unsigned char* data, size_t length) {
    if (length == 0) {
        return 0;
    }

    size_t check_length = length > 1000 ? 1000 : length;

    for (size_t i = 0; i < check_length; i++) {
        if (data[i] == 0) {
            return 1;
        }

        if (data[i] < 9 || (data[i] > 13 && data[i] < 32) || data[i] == 127) {
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

    for (size_t i = 0; i < bytes_to_print; i++) {
        printf("%02X ", buffer->data[i]);
        if ((i + 1) % 16 == 0) {
            printf("\n");
        }
    }

    if (bytes_to_print % 16 != 0) {
        printf("\n");
    }

    if (bytes_to_print < buffer->length) {
        printf("... (еще %zu байт)\n", buffer->length - bytes_to_print);
    }
}

char* copy_string(const char* source) {
    if (!source) {
        return NULL;
    }

    size_t length = strlen(source);
    char* copy = ALLOCATE(char, length + 1);
    if (!copy) {
        return NULL;
    }

    memcpy(copy, source, length);
    copy[length] = '\0';
    return copy;
}

char* format_string(const char* format, ...) {
    va_list args;
    va_start(args, format);

    va_list args_copy;
    va_copy(args_copy, args);

    int length = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);

    if (length < 0) {
        va_end(args);
        return NULL;
    }

    char* buffer = ALLOCATE(char, length + 1);
    if (!buffer) {
        va_end(args);
        return NULL;
    }

    vsnprintf(buffer, length + 1, format, args);
    va_end(args);

    return buffer;
}