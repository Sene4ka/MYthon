#ifndef MYTHON_NATIVE_H
#define MYTHON_NATIVE_H

#include "vm.h"

typedef struct {
    const char* name;
    int arity;
    int global_index;
} NativeInfo;

NativeInfo* native_get_info(const char* name);
int native_count(void);

int native_is_native(const char* name);
int native_get_global_index(const char* name);

void native_register_all(VM* vm);

Value native_print(VM* vm, int arg_count, Value* args);
Value native_len(VM* vm, int arg_count, Value* args);
Value native_array_push(VM* vm, int arg_count, Value* args);
Value native_array_pop(VM* vm, int arg_count, Value* args);
Value native_typeof(VM* vm, int arg_count, Value* args);
Value native_to_string(VM* vm, int arg_count, Value* args);
Value native_to_int(VM* vm, int arg_count, Value* args);
Value native_to_float(VM* vm, int arg_count, Value* args);
Value native_exit(VM* vm, int arg_count, Value* args);
Value native_time(VM* vm, int arg_count, Value* args);
Value native_random(VM* vm, int arg_count, Value* args);
Value native_floor(VM* vm, int arg_count, Value* args);
Value native_ceil(VM* vm, int arg_count, Value* args);
Value native_sqrt(VM* vm, int arg_count, Value* args);
Value native_pow(VM* vm, int arg_count, Value* args);
Value native_trunc(VM* vm, int arg_count, Value* args);

NativeFunctionObject* vm_new_native_function(
    VM* vm, const char* name, NativeFn function, int arity
);

#endif

