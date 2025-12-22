#ifndef MYTHON_NATIVE_H
#define MYTHON_NATIVE_H

#include "core/vm.h"

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

#endif