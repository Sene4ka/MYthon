#include "native.h"
#include "core/vm.h"
#include "utils/memory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

void native_register_all(VM* vm) {
    vm_store_global(vm, 0, OBJECT_VAL(vm_copy_string(vm, "print", 5)));
    vm_store_global(vm, 1, OBJECT_VAL(vm_copy_string(vm, "len", 3)));
    vm_store_global(vm, 2, OBJECT_VAL(vm_copy_string(vm, "type", 4)));
    vm_store_global(vm, 3, OBJECT_VAL(vm_copy_string(vm, "exit", 4)));
    vm_store_global(vm, 4, OBJECT_VAL(vm_copy_string(vm, "time", 4)));
    vm_store_global(vm, 5, OBJECT_VAL(vm_copy_string(vm, "push", 4)));
    vm_store_global(vm, 6, OBJECT_VAL(vm_copy_string(vm, "pop", 3)));
    vm_store_global(vm, 7,  OBJECT_VAL(vm_copy_string(vm, "random", 6)));
}

Value native_print(int arg_count, Value* args) {
    for (int i = 0; i < arg_count; i++) {
        vm_print_value(args[i]);
        if (i < arg_count - 1) {
            printf(" ");
        }
    }
    printf("\n");
    return NIL_VAL;
}

Value native_len(int arg_count, Value* args) {
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

Value native_array_push(int arg_count, Value* args) {
    if (arg_count < 2) return NIL_VAL;
    if (!IS_ARRAY(args[0])) return NIL_VAL;

    ArrayObject* array = AS_ARRAY(args[0]);
    for (int i = 1; i < arg_count; i++) {
        vm_array_append(NULL, array, args[i]);
    }
    return INT_VAL(array->count);
}

Value native_array_pop(int arg_count, Value* args) {
    if (arg_count != 1) return NIL_VAL;
    if (!IS_ARRAY(args[0])) return NIL_VAL;

    ArrayObject* array = AS_ARRAY(args[0]);
    if (array->count == 0) return NIL_VAL;

    return array->items[--array->count];
}

Value native_typeof(int arg_count, Value* args) {
    if (arg_count != 1) {
        return NIL_VAL;
    }

    const char* type_name = vm_value_type_name(args[0]);
    StringObject* str = vm_copy_string(NULL, type_name, strlen(type_name));
    return OBJECT_VAL(str);
}

Value native_to_string(int arg_count, Value* args) {
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

    StringObject* str = vm_copy_string(NULL, buffer, strlen(buffer));
    return OBJECT_VAL(str);
}

Value native_to_int(int arg_count, Value* args) {
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

Value native_to_float(int arg_count, Value* args) {
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

Value native_exit(int arg_count, Value* args) {
    int code = 0;
    if (arg_count >= 1 && IS_INT(args[0])) {
        code = (int)AS_INT(args[0]);
    }
    exit(code);
    return NIL_VAL;
}

Value native_time(int arg_count, Value* args) {
    (void)args;
    if (arg_count != 0) {
        return NIL_VAL;
    }

    return FLOAT_VAL((double)clock() / CLOCKS_PER_SEC);
}

Value native_random(int arg_count, Value* args) {
    (void)args;
    if (arg_count != 0) return NIL_VAL;
    return FLOAT_VAL((double)rand() / RAND_MAX);
}