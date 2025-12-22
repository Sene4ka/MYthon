#ifndef MYTHON_NATIVE_H
#define MYTHON_NATIVE_H

#include "core/vm.h"

typedef struct {
    const char* name;
    int arity;
    int global_index;
} NativeInfo;

NativeInfo* native_get_info(const char* name);

int native_is_native(const char* name);
int native_get_global_index(const char* name);

void native_register_all(VM* vm);

Value native_print(int arg_count, Value* args);
Value native_len(int arg_count, Value* args);
Value native_array_push(int arg_count, Value* args);
Value native_array_pop(int arg_count, Value* args);
Value native_typeof(int arg_count, Value* args);
Value native_to_string(int arg_count, Value* args);
Value native_to_int(int arg_count, Value* args);
Value native_to_float(int arg_count, Value* args);
Value native_exit(int arg_count, Value* args);
Value native_time(int arg_count, Value* args);
Value native_random(int arg_count, Value* args);

NativeFunctionObject* vm_new_native_function(VM* vm, const char* name, NativeFn function, int arity);

#endif
