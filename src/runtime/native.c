#include "native.h"
#include "vm.h"
#include "utils/memory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

static NativeInfo native_functions[] = {
    {"print",  -1, 0},
    {"len",     1, 1},
    {"type",    1, 2},
    {"exit",    1, 3},
    {"time",    0, 4},
    {"push",   -1, 5},
    {"pop",     1, 6},
    {"random",  0, 7},
    {"floor",   1, 8},
    {NULL,      0, -1}
};

NativeInfo* native_get_info(const char* name) {
    for (int i = 0; native_functions[i].name != NULL; i++) {
        if (strcmp(native_functions[i].name, name) == 0) {
            return &native_functions[i];
        }
    }
    return NULL;
}

int native_count(void) {
    int count = 0;
    for (int i = 0; native_functions[i].name != NULL; i++) {
        count++;
    }
    return count;
}

int native_is_native(const char* name) {
    return native_get_info(name) != NULL;
}

int native_get_global_index(const char* name) {
    NativeInfo* info = native_get_info(name);
    return info ? info->global_index : -1;
}

void native_register_all(VM* vm) {
    vm_store_global(vm, 0, OBJECT_VAL(vm_new_native_function(vm, "print",  native_print,  -1)));
    vm_store_global(vm, 1, OBJECT_VAL(vm_new_native_function(vm, "len",    native_len,     1)));
    vm_store_global(vm, 2, OBJECT_VAL(vm_new_native_function(vm, "type",   native_typeof,  1)));
    vm_store_global(vm, 3, OBJECT_VAL(vm_new_native_function(vm, "exit",   native_exit,    1)));
    vm_store_global(vm, 4, OBJECT_VAL(vm_new_native_function(vm, "time",   native_time,    0)));
    vm_store_global(vm, 5, OBJECT_VAL(vm_new_native_function(vm, "push",   native_array_push, -1)));
    vm_store_global(vm, 6, OBJECT_VAL(vm_new_native_function(vm, "pop",    native_array_pop,  1)));
    vm_store_global(vm, 7, OBJECT_VAL(vm_new_native_function(vm, "random", native_random,  0)));
    vm_store_global(vm, 8, OBJECT_VAL(vm_new_native_function(vm, "floor",  native_floor,   1)));
}


Value native_print(VM* vm, int arg_count, Value* args) {
    (void)vm;
    for (int i = 0; i < arg_count; i++) {
        vm_print_value(args[i]);
        if (i < arg_count - 1) {
            printf(" ");
        }
    }
    printf("\n");
    return NIL_VAL;
}

Value native_len(VM* vm, int arg_count, Value* args) {
    (void)vm;
    if (arg_count != 1) {
        return NIL_VAL;
    }

    Value arg = args[0];

    if (IS_STRING(arg)) {
        StringObject* str = AS_STRING(arg);
        return INT_VAL(str->length);
    }

    if (IS_ARRAY(arg)) {
        ArrayObject* arr = AS_ARRAY(arg);
        return INT_VAL(arr->count);
    }

    return INT_VAL(0);
}

Value native_array_push(VM* vm, int arg_count, Value* args) {
    if (arg_count < 2) return NIL_VAL;
    if (!IS_ARRAY(args[0])) return NIL_VAL;

    ArrayObject* array = AS_ARRAY(args[0]);
    for (int i = 1; i < arg_count; i++) {
        vm_array_append(vm, array, args[i]);
    }
    return INT_VAL(array->count);
}

Value native_array_pop(VM* vm, int arg_count, Value* args) {
    (void)vm;
    if (arg_count != 1) return NIL_VAL;
    if (!IS_ARRAY(args[0])) return NIL_VAL;

    ArrayObject* array = AS_ARRAY(args[0]);
    if (array->count == 0) return NIL_VAL;

    return array->items[--array->count];
}

Value native_typeof(VM* vm, int arg_count, Value* args) {
    if (arg_count != 1) {
        return NIL_VAL;
    }

    const char* type_name = vm_value_type_name(args[0]);
    StringObject* str = vm_copy_string(vm, type_name, strlen(type_name));
    return OBJECT_VAL(str);
}

Value native_to_string(VM* vm, int arg_count, Value* args) {
    if (arg_count != 1) {
        return NIL_VAL;
    }

    Value val = args[0];
    char buffer[256];

    if (IS_NIL(val)) {
        snprintf(buffer, sizeof(buffer), "nil");
    } else if (IS_BOOL(val)) {
        snprintf(buffer, sizeof(buffer), "%s", AS_BOOL(val) ? "true" : "false");
    } else if (IS_INT(val)) {
        snprintf(buffer, sizeof(buffer), "%lld", (long long)AS_INT(val));
    } else if (IS_FLOAT(val)) {
        snprintf(buffer, sizeof(buffer), "%g", AS_FLOAT(val));
    } else if (IS_STRING(val)) {
        return val;
    } else {
        const char* type = vm_value_type_name(val);
        snprintf(buffer, sizeof(buffer), "[%s]", type);
    }

    StringObject* str = vm_copy_string(vm, buffer, strlen(buffer));
    return OBJECT_VAL(str);
}

Value native_to_int(VM* vm, int arg_count, Value* args) {
    (void)vm;
    if (arg_count != 1) {
        return NIL_VAL;
    }

    Value val = args[0];

    if (IS_INT(val)) {
        return val;
    }

    if (IS_FLOAT(val)) {
        return INT_VAL((int64_t)AS_FLOAT(val));
    }

    if (IS_STRING(val)) {
        StringObject* str = AS_STRING(val);
        char* end;
        long long result = strtoll(str->chars, &end, 10);
        if (end != str->chars) {
            return INT_VAL(result);
        }
    }

    return INT_VAL(0);
}

Value native_to_float(VM* vm, int arg_count, Value* args) {
    (void)vm;
    if (arg_count != 1) {
        return NIL_VAL;
    }

    Value val = args[0];

    if (IS_FLOAT(val)) {
        return val;
    }

    if (IS_INT(val)) {
        return FLOAT_VAL((double)AS_INT(val));
    }

    if (IS_STRING(val)) {
        StringObject* str = AS_STRING(val);
        char* end;
        double result = strtod(str->chars, &end);
        if (end != str->chars) {
            return FLOAT_VAL(result);
        }
    }

    return FLOAT_VAL(0.0);
}

Value native_exit(VM* vm, int arg_count, Value* args) {
    (void)vm;
    int code = 0;
    if (arg_count >= 1 && IS_INT(args[0])) {
        code = (int)AS_INT(args[0]);
    }
    exit(code);
    return NIL_VAL;
}

Value native_time(VM* vm, int arg_count, Value* args) {
    (void)vm;
    (void)args;
    if (arg_count != 0) {
        return NIL_VAL;
    }
    return FLOAT_VAL((double)clock() / CLOCKS_PER_SEC);
}

Value native_random(VM* vm, int arg_count, Value* args) {
    (void)vm;
    (void)args;
    if (arg_count != 0) return NIL_VAL;
    return FLOAT_VAL((double)rand() / RAND_MAX);
}

Value native_floor(VM* vm, int arg_count, Value* args) {
    (void)vm;
    if (arg_count != 1) {
        return NIL_VAL;
    }

    Value val = args[0];

    if (IS_INT(val)) {
        return val;
    }

    if (IS_FLOAT(val)) {
        return INT_VAL((int64_t)floor(AS_FLOAT(val)));
    }

    return NIL_VAL;
}
