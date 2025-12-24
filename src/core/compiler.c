#include "compiler.h"
#include "memory.h"
#include "utils.h"
#include "native.h"

#include <string.h>
#include <stdio.h>

/* ===== Вспомогалки по массивам ===== */

static void* grow_array_raw(void* ptr, size_t old_count, size_t new_count, size_t elem_size) {
    if (old_count == 0 && new_count > 0) {
        return ALLOCATE(char, new_count * elem_size);
    }
    return GROW_ARRAY(char, ptr, old_count * elem_size, new_count * elem_size);
}

static FunctionState* current_function_state(Compiler* c) {
    if (c->current_function < 0) return NULL;
    return &c->functions[c->current_function];
}

static Bytecode* current_bc(Compiler* compiler, Bytecode* module_bc) {
    FunctionState* fs = current_function_state(compiler);
    return fs ? fs->func_bc : module_bc;
}

static void ensure_local_capacity(LocalTable* locals, int needed) {
    if (locals->capacity >= needed) return;
    int old = locals->capacity;
    int new_cap = old < 8 ? 8 : old * 2;
    if (new_cap < needed) new_cap = needed;

    locals->names = (char**)grow_array_raw(locals->names, old, new_cap, sizeof(char*));
    locals->depths = (int*)grow_array_raw(locals->depths, old, new_cap, sizeof(int));
    locals->is_captured = (int*)grow_array_raw(locals->is_captured, old, new_cap, sizeof(int));
    locals->capacity = new_cap;
}

static void ensure_upvalue_capacity(FunctionState* fs, int needed) {
    if (fs->upvalue_capacity >= needed) return;
    int old = fs->upvalue_capacity;
    int new_cap = old < 8 ? 8 : old * 2;
    if (new_cap < needed) new_cap = needed;

    fs->upvalues = (UpvalueEntry*)grow_array_raw(fs->upvalues, old, new_cap, sizeof(UpvalueEntry));
    fs->upvalue_capacity = new_cap;
}

static void ensure_function_capacity(Compiler* c, int needed) {
    if (c->function_capacity >= needed) return;
    int old = c->function_capacity;
    int new_cap = old < 4 ? 4 : old * 2;
    if (new_cap < needed) new_cap = needed;

    c->functions = (FunctionState*)grow_array_raw(c->functions, old, new_cap, sizeof(FunctionState));
    c->function_capacity = new_cap;
}

/* ===== Compiler lifecycle ===== */

void compiler_init(Compiler* compiler, const char* source_file) {
    memset(compiler, 0, sizeof(*compiler));
    compiler->source_file = source_file;
    compiler->current_line = 1;

    compiler->functions = NULL;
    compiler->function_depth = 0;
    compiler->function_capacity = 0;
    compiler->current_function = -1;

    compiler->loop.loop_depth = 0;
    compiler->loop.break_jumps = NULL;
    compiler->loop.break_count = 0;
    compiler->loop.break_capacity = 0;
    compiler->loop.continue_jumps = NULL;
    compiler->loop.continue_count = 0;
    compiler->loop.continue_capacity = 0;
    compiler->native_global_offset = native_count();
}

void compiler_free(Compiler* compiler) {
    if (!compiler) return;

    for (int i = 0; i < compiler->function_depth; i++) {
        FunctionState* fs = &compiler->functions[i];
        for (int j = 0; j < fs->locals.count; j++) {
            if (fs->locals.names && fs->locals.names[j]) {
                FREE_ARRAY(char, fs->locals.names[j], strlen(fs->locals.names[j]) + 1);
            }
        }
        FREE_ARRAY(char*, fs->locals.names, fs->locals.capacity);
        FREE_ARRAY(int, fs->locals.depths, fs->locals.capacity);
        FREE_ARRAY(int, fs->locals.is_captured, fs->locals.capacity);

        if (fs->name) {
            FREE_ARRAY(char, fs->name, strlen(fs->name) + 1);
        }
        FREE_ARRAY(UpvalueEntry, fs->upvalues, fs->upvalue_capacity);
    }
    FREE_ARRAY(FunctionState, compiler->functions, compiler->function_capacity);

    FREE_ARRAY(size_t, compiler->loop.break_jumps, compiler->loop.break_capacity);
    FREE_ARRAY(size_t, compiler->loop.continue_jumps, compiler->loop.continue_capacity);
}

/* ===== Ошибки ===== */

void compiler_error(Compiler* compiler, const char* message) {
    compiler->error_count++;
    compiler->error_message = message;
    fprintf(stderr, "Compile error: %s\n", message);
}

void compiler_error_at_line(Compiler* compiler, int line, const char* message) {
    compiler->error_count++;
    compiler->error_message = message;
    fprintf(stderr, "%s:%d: compile error: %s\n",
            compiler->source_file ? compiler->source_file : "<source>",
            line, message);
}

/* ===== Работа с Bytecode: базовые emit_* ===== */

void emit_byte(Compiler* compiler, Bytecode* bytecode, uint8_t byte) {
    bc_write_u8(bytecode, byte, compiler->current_line);
}

void emit_op(Compiler* compiler, Bytecode* bytecode, OpCode op) {
    bc_write_op(bytecode, op, compiler->current_line);
}

void emit_u8(Compiler* compiler, Bytecode* bytecode, uint8_t value) {
    bc_write_u8(bytecode, value, compiler->current_line);
}

void emit_u16(Compiler* compiler, Bytecode* bytecode, uint16_t value) {
    bc_write_u16(bytecode, value, compiler->current_line);
}

/* константы */
int compiler_add_const_int(Bytecode* bytecode, int64_t value) {
    return bc_add_int(bytecode, value);
}

int compiler_add_const_float(Bytecode* bytecode, double value) {
    return bc_add_float(bytecode, value);
}

int compiler_add_const_string(Bytecode* bytecode, const char* value) {
    return bc_add_string(bytecode, value);
}
void emit_jump_u16(Compiler* compiler, Bytecode* bytecode,
                   OpCode op, size_t* patch_pos) {
    emit_op(compiler, bytecode, op);
    *patch_pos = bytecode->code_size;
    bc_write_u16(bytecode, 0, compiler->current_line);
}

void patch_jump_u16(Bytecode* bytecode, size_t patch_pos) {
    size_t jump_from = patch_pos + 2;
    size_t jump_to = bytecode->code_size;
    int32_t offset = (int32_t)(jump_to - jump_from);

    bytecode->code[patch_pos]     = (uint8_t)((offset >> 8) & 0xFF);
    bytecode->code[patch_pos + 1] = (uint8_t)(offset & 0xFF);
}

void emit_loop_u16(Compiler* compiler, Bytecode* bytecode, size_t loop_start) {
    emit_op(compiler, bytecode, OP_JUMP_U16);
    size_t patch_pos = bytecode->code_size;
    size_t jump_from = patch_pos + 2;
    int32_t offset = (int32_t)(loop_start - jump_from);
    bc_write_u16(bytecode, (uint16_t)offset, compiler->current_line);
}

int compiler_add_local(Compiler* compiler, const char* name, int length) {
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return -1;

    LocalTable* locals = &fs->locals;
    ensure_local_capacity(locals, locals->count + 1);

    char* copy = ALLOCATE(char, length + 1);
    memcpy(copy, name, length);
    copy[length] = '\0';

    int idx = locals->count++;
    locals->names[idx] = copy;
    locals->depths[idx] = fs->scope_depth;
    locals->is_captured[idx] = 0;

    if (locals->count > fs->max_locals) {
        fs->max_locals = locals->count;
    }

    return idx;
}

int compiler_resolve_local(Compiler* compiler, const char* name, int length) {
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return -1;

    LocalTable* locals = &fs->locals;
    for (int i = locals->count - 1; i >= 0; i--) {
        if ((int)strlen(locals->names[i]) == length &&
            memcmp(locals->names[i], name, length) == 0) {
            return i;
        }
    }
    return -1;
}

/* поиск/создание upvalue, рекурсивно поднимаясь по стекам функций */
int compiler_resolve_upvalue(Compiler* compiler, const char* name, int length) {
    if (compiler->function_depth <= 1) {
        /* нет внешней функции */
        return -1;
    }

    int outer_index = compiler->current_function - 1;
    /* сначала пробуем найти локал во внешней функции */
    Compiler outer_tmp = *compiler;
    outer_tmp.current_function = outer_index;
    FunctionState* outer_fs = &compiler->functions[outer_index];

    int local = compiler_resolve_local(&outer_tmp, name, length);
    if (local >= 0) {
        /* помечаем локал как захваченный */
        outer_fs->locals.is_captured[local] = 1;

        FunctionState* fs = current_function_state(compiler);
        /* проверим, не существует ли уже такой upvalue */
        for (int i = 0; i < fs->upvalue_count; i++) {
            UpvalueEntry* uv = &fs->upvalues[i];
            if (uv->is_local && uv->index == local) {
                return i;
            }
        }
        ensure_upvalue_capacity(fs, fs->upvalue_count + 1);
        int idx = fs->upvalue_count++;
        fs->upvalues[idx].is_local = 1;
        fs->upvalues[idx].index = local;
        return idx;
    }

    int up = compiler_resolve_upvalue(&outer_tmp, name, length);
    if (up >= 0) {
        FunctionState* fs = current_function_state(compiler);
        for (int i = 0; i < fs->upvalue_count; i++) {
            UpvalueEntry* uv = &fs->upvalues[i];
            if (!uv->is_local && uv->index == up) {
                return i;
            }
        }
        ensure_upvalue_capacity(fs, fs->upvalue_count + 1);
        int idx = fs->upvalue_count++;
        fs->upvalues[idx].is_local = 0;
        fs->upvalues[idx].index = up;
        return idx;
    }

    return -1;
}

void compiler_begin_scope(Compiler* compiler) {
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return;
    fs->scope_depth++;
}

void compiler_end_scope(Compiler* compiler, Bytecode* bytecode, int start_local_count) {
    (void)bytecode; /* пока scope только логический, локалы не трогаем на уровне байткода */
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return;

    fs->scope_depth--;
    LocalTable* locals = &fs->locals;

    /* забудем локалы, глубина которых > текущей */
    while (locals->count > start_local_count &&
           locals->depths[locals->count - 1] > fs->scope_depth) {
        locals->count--;
    }
}

/* ===== loops ===== */

void compiler_begin_loop(Compiler* compiler) {
    compiler->loop.loop_depth++;
}

void compiler_end_loop(Compiler* compiler, Bytecode* bytecode) {
    (void)bytecode;
    if (compiler->loop.loop_depth > 0) {
        compiler->loop.loop_depth--;
    }
}

/* patch всех break/continue в текущем модуле до target_ip */
void compiler_patch_breaks(Compiler* compiler, Bytecode* bytecode, size_t target_ip) {
    for (int i = 0; i < compiler->loop.break_count; i++) {
        size_t patch_pos = compiler->loop.break_jumps[i];
        size_t jump_from = patch_pos + 2;
        int32_t offset = (int32_t)(target_ip - jump_from);

        bytecode->code[patch_pos]     = (uint8_t)((offset >> 8) & 0xFF);
        bytecode->code[patch_pos + 1] = (uint8_t)(offset & 0xFF);
    }
    compiler->loop.break_count = 0;
}

void compiler_patch_continues(Compiler* compiler, Bytecode* bytecode, size_t target_ip) {
    for (int i = 0; i < compiler->loop.continue_count; i++) {
        size_t patch_pos = compiler->loop.continue_jumps[i];
        size_t jump_from = patch_pos + 2;
        int32_t offset = (int32_t)(target_ip - jump_from);

        bytecode->code[patch_pos]     = (uint8_t)((offset >> 8) & 0xFF);
        bytecode->code[patch_pos + 1] = (uint8_t)(offset & 0xFF);
    }
    compiler->loop.continue_count = 0;
}

/* ===== functions (main + вложенные) ===== */
void compiler_begin_function(Compiler* compiler,
                             Bytecode* bytecode,
                             const char* name,
                             int arity,
                             int line,
                             int* out_func_index) {
    (void)line; /* можно использовать для ошибок/отладочной информации */

    ensure_function_capacity(compiler, compiler->function_depth + 1);
    int idx = compiler->function_depth++;
    compiler->current_function = idx;

    FunctionState* fs = &compiler->functions[idx];
    memset(fs, 0, sizeof(*fs));

    if (name) {
        size_t len = strlen(name);
        fs->name = ALLOCATE(char, len + 1);
        memcpy(fs->name, name, len + 1);
    } else {
        fs->name = NULL;
    }

    fs->arity = arity;
    fs->max_locals = 0;
    fs->scope_depth = 0;

    fs->locals.names = NULL;
    fs->locals.depths = NULL;
    fs->locals.is_captured = NULL;
    fs->locals.count = 0;
    fs->locals.capacity = 0;

    fs->upvalues = NULL;
    fs->upvalue_count = 0;
    fs->upvalue_capacity = 0;

    fs->func_bc = bytecode_new();
    fs->start_ip = 0;
    fs->end_ip   = 0;

    fs->func_index = (int)bc_start_function(
        bytecode,
        fs->name ? fs->name : "<fn>",
        arity,
        (uint16_t)0,          // max_locals пока 0, исправим в end_function
        (uint16_t)0           // max_stack пока 0
    );

    if (out_func_index) {
        *out_func_index = fs->func_index;
    }
}

void compiler_end_function(Compiler* compiler, Bytecode* bytecode) {
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return;

    Bytecode* fbc = fs->func_bc;
    if (!fbc) return;

    // Начало кода функции в модульном байткоде
    uint32_t code_start = (uint32_t)bytecode->code_size;

    // Копируем код функции в общий bytecode
    for (size_t i = 0; i < fbc->code_size; i++) {
        // Можно переносить и line_numbers, если нужно
        bc_write_u8(bytecode,
                    fbc->code[i],
                    fbc->line_numbers ? fbc->line_numbers[i]
                                      : compiler->current_line);
    }

    uint32_t code_end = (uint32_t)bytecode->code_size;
    fs->start_ip = (int)code_start;
    fs->end_ip   = (int)code_end;

    // Переносим upvalues
    for (int i = 0; i < fs->upvalue_count; i++) {
        UpvalueEntry* uv = &fs->upvalues[i];
        bc_add_upvalue(bytecode,
                       (uint32_t)fs->func_index,
                       (uint16_t)uv->index,
                       uv->is_local ? true : false);
    }

    // Завершаем функцию в модульном байткоде
    Function* fn = &bytecode->functions[fs->func_index];
    fn->code_start = code_start;
    fn->code_end   = code_end;
    fn->max_locals = (uint16_t)fs->max_locals;
    // max_stack пока 0 или считаешь отдельно

    bc_end_function(bytecode, (uint32_t)fs->func_index);

    // Освобождаем временный func_bc
    bytecode_free(fbc);
    fs->func_bc = NULL;

    compiler->function_depth--;
    compiler->current_function = compiler->function_depth - 1;
}

int compiler_resolve_global(Compiler* compiler, Bytecode* bytecode, const char* name) {
    int raw = bc_resolve_global(bytecode, name); // 0,1,2,... внутри Bytecode
    if (raw < 0) return -1;
    return compiler->native_global_offset + raw;
}

int compiler_define_global(Compiler* compiler, Bytecode* bytecode, const char* name, int const_idx) {
    int raw = bc_define_global(bytecode, name, const_idx); // 0,1,2,...
    if (raw < 0) return -1;
    return compiler->native_global_offset + raw;
}

/* ===== Основной entrypoint компиляции ===== */
CompileResult compiler_compile(Compiler* compiler, const ASTNode* ast, Bytecode* bytecode) {
    fprintf(stderr, "DEBUG: compile start root\n");
    compiler->error_count = 0;

    int main_index = -1;
    compiler_begin_function(compiler, bytecode, NULL, 0, 1, &main_index);

    fprintf(stderr, "DEBUG: before compile_program\n");
    CompileResult result = compile_program(compiler, ast, bytecode);
    fprintf(stderr, "DEBUG: after compile_program, result=%d, errors=%d\n",
            result, compiler->error_count);

    if (compiler->error_count == 0) {
        Bytecode* bc = current_bc(compiler, bytecode);
        emit_op(compiler, bc, OP_RETURN_NIL);
        compiler_end_function(compiler, bytecode);
        fprintf(stderr, "DEBUG: end root OK\n");
        return result;
    }

    fprintf(stderr, "DEBUG: end root with errors\n");
    return COMPILE_ERROR;
}


CompileResult compile_expression(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    return compile_node(compiler, node, bytecode);
}

CompileResult compile_node(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node) return COMPILE_SUCCESS;

    switch (node->type) {
        /* выражения */
        case NODE_LITERAL_EXPR:   return compile_literal(compiler, node, bytecode);
        case NODE_VARIABLE_EXPR:  return compile_variable(compiler, node, bytecode);
        case NODE_UNARY_EXPR:     return compile_unary(compiler, node, bytecode);
        case NODE_BINARY_EXPR:    return compile_binary(compiler, node, bytecode);
        case NODE_LOGICAL_EXPR:   return compile_logical(compiler, node, bytecode);
        case NODE_ASSIGN_EXPR:    return compile_assign(compiler, node, bytecode);
        case NODE_CALL_EXPR:      return compile_call(compiler, node, bytecode);
        case NODE_ARRAY_EXPR:     return compile_array(compiler, node, bytecode);
        case NODE_INDEX_EXPR:     return compile_index(compiler, node, bytecode);
        case NODE_TERNARY_EXPR:   return compile_ternary(compiler, node, bytecode);
        case NODE_GROUP_EXPR:
            return compile_expression(compiler, node->unary.operand, bytecode);

        case NODE_MEMBER_EXPR:
            return compile_member(compiler, node, bytecode); /* пока может выдавать ошибку */

        /* стейтменты */
        case NODE_EXPR_STMT:
        case NODE_BLOCK_STMT:
        case NODE_IF_STMT:
        case NODE_FOR_STMT:
        case NODE_RETURN_STMT:
        case NODE_FUNCTION_STMT:
        case NODE_BREAK_STMT:
        case NODE_CONTINUE_STMT:
            return compile_statement(compiler, node, bytecode);

        case NODE_PROGRAM:
            return compile_program(compiler, node, bytecode);

        default:
            compiler_error_at_line(compiler, node->line, "Unsupported node type (yet)");
            return COMPILE_ERROR;
    }
}

CompileResult compile_statement(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    switch (node->type) {
        case NODE_EXPR_STMT: {
            Bytecode* bc = current_bc(compiler, bytecode);
            CompileResult r = compile_expression(compiler, node->expr_stmt.expression, bytecode);
            if (r != COMPILE_SUCCESS) return r;
            emit_op(compiler, bc, OP_POP);
            return COMPILE_SUCCESS;
        }
        case NODE_BLOCK_STMT:
            return compile_block(compiler, node, bytecode);

        case NODE_IF_STMT:
            return compile_if(compiler, node, bytecode);

        case NODE_FOR_STMT:
            return compile_for(compiler, node, bytecode);

        case NODE_RETURN_STMT:
            return compile_return(compiler, node, bytecode);

        case NODE_FUNCTION_STMT:
            return compile_function(compiler, node, bytecode);

        case NODE_BREAK_STMT:
            return compile_break(compiler, bytecode);

        case NODE_CONTINUE_STMT:
            return compile_continue(compiler, bytecode);

        default:
            fprintf(stderr, "DEBUG: compile_statement unsupported type=%d at line %d\n",
                    node->type, node->line);
            compiler_error_at_line(compiler, node->line, "Unsupported statement type");
            return COMPILE_ERROR;
    }
}

CompileResult compile_program(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_PROGRAM) {
        compiler_error(compiler, "Expected program node");
        return COMPILE_ERROR;
    }
    ASTNode* stmt = node->program.statements;
    while (stmt) {
        CompileResult r = compile_node(compiler, stmt, bytecode);
        if (r != COMPILE_SUCCESS) return r;
        stmt = stmt->next;
    }
    return COMPILE_SUCCESS;
}

CompileResult compile_block(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_BLOCK_STMT) {
        compiler_error(compiler, "Expected block statement");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    FunctionState* fs = current_function_state(compiler);
    int start_locals = fs ? fs->locals.count : 0;

    compiler_begin_scope(compiler);

    ASTNode* stmt = node->block.statements;
    while (stmt) {
        CompileResult r = compile_statement(compiler, stmt, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_end_scope(compiler, bc, start_locals);
            return r;
        }
        stmt = stmt->next;
    }

    compiler_end_scope(compiler, bc, start_locals);
    return COMPILE_SUCCESS;
}


CompileResult compile_unary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_UNARY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected unary expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    CompileResult r = compile_expression(compiler, node->unary.operand, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    OpCode op = get_unary_opcode(node->unary.operator);
    if (op == OP_NOP) {
        compiler_error_at_line(compiler, node->line, "Unsupported unary operator");
        return COMPILE_ERROR;
    }

    emit_op(compiler, bc, op);
    return COMPILE_SUCCESS;
}

CompileResult compile_binary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_BINARY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected binary expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    CompileResult r = compile_expression(compiler, node->binary.left, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    r = compile_expression(compiler, node->binary.right, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    OpCode op = get_binary_opcode(node->binary.operator);
    if (op == OP_NOP) {
        compiler_error_at_line(compiler, node->line, "Unsupported binary operator");
        return COMPILE_ERROR;
    }

    emit_op(compiler, bc, op);
    return COMPILE_SUCCESS;
}

CompileResult compile_postfix(Compiler* c, const ASTNode* n, Bytecode* b){(void)c;(void)n;(void)b;return COMPILE_ERROR;}

CompileResult compile_literal(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_LITERAL_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected literal expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    int const_idx = -1;

    switch (node->literal.value_type) {
        case LIT_NUMBER: {
            double value = node->literal.value.number;
            int64_t iv = (int64_t)value;
            if ((double)iv == value) {
                const_idx = compiler_add_const_int(bytecode, iv);
            } else {
                const_idx = compiler_add_const_float(bytecode, value);
            }
            break;
        }
        case LIT_STRING: {
            const char* s = node->literal.value.string;
            const_idx = compiler_add_const_string(bytecode, s ? s : "");
            break;
        }
        case LIT_BOOL: {
            const_idx = compiler_add_const_int(bytecode,
                                               node->literal.value.boolean ? 1 : 0);
            break;
        }
        case LIT_NIL: {
            /* nil можно закодировать как int 0 с особой трактовкой в VM или
               позже добавить специальную константу; пока используем 0. */
            const_idx = compiler_add_const_int(bytecode, 0);
            break;
        }
        default:
            compiler_error_at_line(compiler, node->line, "Unknown literal type");
            return COMPILE_ERROR;
    }

    if (const_idx < 0) {
        compiler_error_at_line(compiler, node->line, "Failed to add constant");
        return COMPILE_ERROR;
    }

    emit_op(compiler, bc, OP_LOAD_CONST_U16);
    emit_u16(compiler, bc, (uint16_t)const_idx);
    return COMPILE_SUCCESS;
}
CompileResult compile_variable(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_VARIABLE_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected variable expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    const char* name = node->variable.name;
    int length = (int)node->variable.name_length;

    /* 1) локал */
    int local = compiler_resolve_local(compiler, name, length);
    if (local >= 0) {
        if (local <= 0xFF) {
            emit_op(compiler, bc, OP_LOAD_LOCAL_U8);
            emit_u8(compiler, bc, (uint8_t)local);
        } else {
            compiler_error_at_line(compiler, node->line, "Too many locals");
            return COMPILE_ERROR;
        }
        return COMPILE_SUCCESS;
    }

    /* 2) upvalue */
    int up = compiler_resolve_upvalue(compiler, name, length);
    if (up >= 0) {
        if (up <= 0xFF) {
            emit_op(compiler, bc, OP_LOAD_UPVALUE_U8);
            emit_u8(compiler, bc, (uint8_t)up);
        } else {
            compiler_error_at_line(compiler, node->line, "Too many upvalues");
            return COMPILE_ERROR;
        }
        return COMPILE_SUCCESS;
    }

    NativeInfo* native_info = native_get_info(name);
    if (native_info != NULL) {
        int global_index = native_get_global_index(name);
        if (global_index < 0) {
            char buf[256];
            snprintf(buf, sizeof(buf), "Native '%s' not registered", name);
            compiler_error_at_line(compiler, node->line, buf);
            return COMPILE_ERROR;
        }
        emit_op(compiler, bc, OP_LOAD_GLOBAL_U16);
        emit_u16(compiler, bc, (uint16_t)global_index);
        return COMPILE_SUCCESS;
    }

    int global = compiler_resolve_global(compiler, bytecode, name);
    if (global >= 0) {
        emit_op(compiler, bc, OP_LOAD_GLOBAL_U16);
        emit_u16(compiler, bc, (uint16_t)global);
        return COMPILE_SUCCESS;
    }

    compiler_error_at_line(compiler, node->line, "Unknown variable");
    return COMPILE_ERROR;
}

CompileResult compile_assign(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_ASSIGN_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected assignment expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    const ASTNode* target = node->assign.target;
    const ASTNode* value  = node->assign.value;

    /* сначала вычисляем правую часть */
    CompileResult r = compile_expression(compiler, value, bytecode);
    if (r != COMPILE_SUCCESS) return r;

        if (target->type == NODE_VARIABLE_EXPR) {
        const char* name = target->variable.name;
        int length = (int)target->variable.name_length;

        /* 1) локал */
        int local = compiler_resolve_local(compiler, name, length);
        if (local >= 0) {
            if (local > 0xFF) {
                compiler_error_at_line(compiler, node->line, "Too many locals");
                return COMPILE_ERROR;
            }
            emit_op(compiler, bc, OP_STORE_LOCAL_U8);
            emit_u8(compiler, bc, (uint8_t)local);
            return COMPILE_SUCCESS;
        }

        /* 2) upvalue (присваивание в замыкание) */
        int up = compiler_resolve_upvalue(compiler, name, length);
        if (up >= 0) {
            if (up > 0xFF) {
                compiler_error_at_line(compiler, node->line, "Too many upvalues");
                return COMPILE_ERROR;
            }
            emit_op(compiler, bc, OP_STORE_UPVALUE_U8);
            emit_u8(compiler, bc, (uint8_t)up);
            return COMPILE_SUCCESS;
        }

        /* 3) глобал: создаём или обновляем */
        char buf[256];
        int name_len = length < (int)sizeof(buf)-1 ? length : (int)sizeof(buf)-1;
        memcpy(buf, name, name_len);
        buf[name_len] = '\0';

        int global = compiler_resolve_global(compiler, bytecode, buf);
        if (global < 0) {
            /* Если хочешь поведение let/var, можно: при первом присваивании
               считать объявлением и завести глобал с начальным значением. */
            int const_idx = -1;
            global = compiler_define_global(compiler, bytecode, buf, const_idx);
            if (global < 0) {
                compiler_error_at_line(compiler, node->line, "Failed to define global");
                return COMPILE_ERROR;
            }
        }

        emit_op(compiler, bc, OP_STORE_GLOBAL_U16);
        emit_u16(compiler, bc, (uint16_t)global);
        return COMPILE_SUCCESS;
    }

    if (target->type == NODE_INDEX_EXPR) {
        /* arr[index] = value:
           стек к моменту OP_ARRAY_SET должен быть: arr, index, value.
           Сейчас на стеке только value -> надо собрать arr и index ПЕРЕД value.
           Проще пересобрать всё: arr, index, value. */

        /* убираем value со стека, чтобы не дублировать.
           Пока нет специального dup, проще сделать:
           - перестроить выражение так, чтобы сначала arr/index, потом value.
           Для простоты: пере‑компилируем по порядку arr, index, value. */

        /* Оптимизацией займёмся позже; сейчас просто три expression. */
        /* Очистим value: POP */
        emit_op(compiler, bc, OP_POP);

        r = compile_expression(compiler, target->index.array, bytecode);
        if (r != COMPILE_SUCCESS) return r;

        r = compile_expression(compiler, target->index.index, bytecode);
        if (r != COMPILE_SUCCESS) return r;

        r = compile_expression(compiler, value, bytecode);
        if (r != COMPILE_SUCCESS) return r;

        emit_op(compiler, bc, OP_ARRAY_SET);
        return COMPILE_SUCCESS;
    }

    compiler_error_at_line(compiler, node->line, "Invalid assignment target");
    return COMPILE_ERROR;
}

CompileResult compile_call(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_CALL_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected call expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    const ASTNode* callee = node->call.callee;

    /* сначала callee */
    CompileResult r = compile_expression(compiler, callee, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    /* затем аргументы */
    for (size_t i = 0; i < node->call.arg_count; i++) {
        r = compile_expression(compiler, node->call.arguments[i], bytecode);
        if (r != COMPILE_SUCCESS) return r;
    }

    size_t argc = node->call.arg_count;
    if (argc <= 0xFF) {
        emit_op(compiler, bc, OP_CALL_U8);
        emit_u8(compiler, bc, (uint8_t)argc);
    } else {
        emit_op(compiler, bc, OP_CALL_U16);
        emit_u16(compiler, bc, (uint16_t)argc);
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_logical(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_LOGICAL_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected logical expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    TokenType op = node->logical.operator;

    /* left */
    CompileResult r = compile_expression(compiler, node->logical.left, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    size_t jump_pos = 0;

    if (op == TOKEN_OR) {
        /* OR: если left true — перескакиваем right */
        emit_jump_u16(compiler, bc, OP_JUMP_IF_TRUE_U16, &jump_pos);
    } else {
        /* AND: если left false — перескакиваем right */
        emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &jump_pos);
    }

    r = compile_expression(compiler, node->logical.right, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    patch_jump_u16(bc, jump_pos);
    return COMPILE_SUCCESS;
}

CompileResult compile_array(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_ARRAY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected array expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    /* сначала все элементы на стек */
    for (size_t i = 0; i < node->array.element_count; i++) {
        CompileResult r = compile_expression(compiler, node->array.elements[i], bytecode);
        if (r != COMPILE_SUCCESS) return r;
    }

    if (node->array.element_count > 0xFF) {
        compiler_error_at_line(compiler, node->line, "Too many array elements");
        return COMPILE_ERROR;
    }

    emit_op(compiler, bc, OP_ARRAY_NEW_U8);
    emit_u8(compiler, bc, (uint8_t)node->array.element_count);
    return COMPILE_SUCCESS;
}

CompileResult compile_index(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_INDEX_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected index expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    CompileResult r = compile_expression(compiler, node->index.array, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    r = compile_expression(compiler, node->index.index, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    emit_op(compiler, bc, OP_ARRAY_GET);
    return COMPILE_SUCCESS;
}

CompileResult compile_member(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    (void)compiler; (void)node; (void)bytecode;
    compiler_error_at_line(compiler, node ? node->line : 0, "Member expressions not yet implemented");
    return COMPILE_ERROR;
}

CompileResult compile_ternary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_TERNARY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected ternary expression");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    /* condition */
    CompileResult r = compile_expression(compiler, node->ternary.condition, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    size_t else_jump = 0;
    emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &else_jump);

    /* then */
    r = compile_expression(compiler, node->ternary.then_expr, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    size_t end_jump = 0;
    emit_jump_u16(compiler, bc, OP_JUMP_U16, &end_jump);

    patch_jump_u16(bc, else_jump);

    /* else */
    r = compile_expression(compiler, node->ternary.else_expr, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    patch_jump_u16(bc, end_jump);
    return COMPILE_SUCCESS;
}

CompileResult compile_if(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_IF_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected if statement");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    // 1. Скомпилировать условие (оставляет bool на вершине стека)
    CompileResult r = compile_expression(compiler, node->if_stmt.condition, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    // 2. Прыжок, если false
    size_t else_jump = 0;
    emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &else_jump);

    // 3. THEN-ветка: убрать условие
    emit_op(compiler, bc, OP_POP);  // *** добавить POP здесь ***

    r = compile_statement(compiler, node->if_stmt.then_branch, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    size_t end_jump = 0;
    if (node->if_stmt.else_branch) {
        emit_jump_u16(compiler, bc, OP_JUMP_U16, &end_jump);
    }

    // 4. Патч прыжка на else
    patch_jump_u16(bc, else_jump);

    if (node->if_stmt.else_branch) {
        // 5. ELSE-ветка: убрать условие для ветки, в которую прыгнули
        emit_op(compiler, bc, OP_POP);  // *** и здесь тоже POP ***

        r = compile_statement(compiler, node->if_stmt.else_branch, bytecode);
        if (r != COMPILE_SUCCESS) return r;
        patch_jump_u16(bc, end_jump);
    } else {
        // if без else: в false-ветке тоже нужно убрать условие
        emit_op(compiler, bc, OP_POP);  // *** POP после патча, если else нет ***
    }

    return COMPILE_SUCCESS;
}


CompileResult compile_for(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_FOR_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected for statement");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    compiler_begin_scope(compiler);

    /* init */
    /* init: парсер кладёт сюда Assign (NODE_ASSIGN_EXPR), это выражение, а не стейтмент */
    if (node->for_stmt.initializer) {
        CompileResult r = compile_expression(compiler, node->for_stmt.initializer, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_end_scope(compiler, bc,
                               current_function_state(compiler)->locals.count);
            return r;
        }
        emit_op(compiler, bc, OP_POP);
    }

    size_t loop_start = bc->code_size;

    /* condition */
    size_t exit_jump = 0;
    if (node->for_stmt.condition) {
        CompileResult r = compile_expression(compiler, node->for_stmt.condition, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);
            return r;
        }
        emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &exit_jump);
    }

    /* тело + учёт break/continue */
    compiler_begin_loop(compiler);

    CompileResult r = compile_statement(compiler, node->for_stmt.body, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_end_loop(compiler, bc);
        compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);
        return r;
    }

    /* increment */
    if (node->for_stmt.increment) {
        r = compile_expression(compiler, node->for_stmt.increment, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_end_loop(compiler, bc);
            compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);
            return r;
        }
        emit_op(compiler, bc, OP_POP);
    }

    emit_loop_u16(compiler, bc, loop_start);

    size_t end_ip = bc->code_size;
    compiler_patch_continues(compiler, bc, loop_start);
    compiler_patch_breaks(compiler, bc, end_ip);

    if (node->for_stmt.condition) {
        patch_jump_u16(bc, exit_jump);
    }

    compiler_end_loop(compiler, bc);
    compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);
    return COMPILE_SUCCESS;
}

CompileResult compile_function(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_FUNCTION_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected function statement");
        return COMPILE_ERROR;
    }

    const char* name = node->function_stmt.name;
    size_t name_len  = node->function_stmt.name_length;

    /* 1) объявляем имя функции как локальную переменную в текущем scope */
    int local_index = -1;
    if (name && name_len > 0) {
        local_index = compiler_resolve_local(compiler, name, (int)name_len);
        if (local_index < 0) {
            local_index = compiler_add_local(compiler, name, (int)name_len);
            if (local_index < 0) {
                compiler_error_at_line(compiler, node->line, "Failed to add function name as local");
                return COMPILE_ERROR;
            }
        }
        if (local_index > 0xFF) {
            compiler_error_at_line(compiler, node->line, "Too many locals");
            return COMPILE_ERROR;
        }
    }

    /* 2) начинаем новую функцию */
    int func_index = -1;
    compiler_begin_function(compiler,
                            bytecode,                 // модуль
                            name ? name : "<fn>",
                            (int)node->function_stmt.param_count,
                            node->line,
                            &func_index);

    FunctionState* fs = current_function_state(compiler);

    /* 3) параметры как локалы функции */
    for (size_t i = 0; i < node->function_stmt.param_count; i++) {
        const char* param_name = node->function_stmt.parameters[i];
        int param_len = (int)strlen(param_name);
        int idx = compiler_add_local(compiler, param_name, param_len);
        if (idx < 0) {
            compiler_error_at_line(compiler, node->line, "Failed to add function parameter");
            return COMPILE_ERROR;
        }
    }

    /* тело функции — блок (эмитит в fs->func_bc через current_bc) */
    CompileResult r = compile_block(compiler, node->function_stmt.body, bytecode);
    if (r != COMPILE_SUCCESS) return r;

    /* гарантируем return nil, если пользователь не добавил return */
    Bytecode* fbc = current_bc(compiler, bytecode);
    emit_op(compiler, fbc, OP_RETURN_NIL);

    /* завершаем функцию: перенесёт upvalues и скопирует код в модульный bytecode */
    compiler_end_function(compiler, bytecode);

    /* ВАЖНО: после end_function мы уже во внешней функции, current_bc сменился */
    Bytecode* bc = current_bc(compiler, bytecode);

    /* 4) в родительской функции: создаём константу CLOSURE для этой функции */
    uint16_t upcount = (uint16_t)fs->upvalue_count;
    int closure_const = bc_add_closure(bytecode, (uint32_t)func_index, upcount);
    if (closure_const < 0) {
        compiler_error_at_line(compiler, node->line, "Failed to add closure constant");
        return COMPILE_ERROR;
    }

    /* 5) создаём новый closure в рантайме */
    emit_op(compiler, bc, OP_LOAD_CONST_U16);
    emit_u16(compiler, bc, (uint16_t)closure_const);

    /* OP_NEW_CLOSURE: VM возьмёт константу CLOSURE и подготовит upvalues */
    emit_op(compiler, bc, OP_NEW_CLOSURE);

    /* 6) сохраняем closure в локал с именем функции (если имя есть) */
    if (local_index >= 0) {
        emit_op(compiler, bc, OP_STORE_LOCAL_U8);
        emit_u8(compiler, bc, (uint8_t)local_index);
    }

    return COMPILE_SUCCESS;
}


CompileResult compile_return(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_RETURN_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected return statement");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    if (node->return_stmt.value) {
        CompileResult r = compile_expression(compiler, node->return_stmt.value, bytecode);
        if (r != COMPILE_SUCCESS) return r;
        emit_op(compiler, bc, OP_RETURN);
    } else {
        emit_op(compiler, bc, OP_RETURN_NIL);
    }

    return COMPILE_SUCCESS;
}


CompileResult compile_break(Compiler* compiler, Bytecode* bytecode) {
    if (compiler->loop.loop_depth <= 0) {
        compiler_error(compiler, "break used outside of loop");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    emit_op(compiler, bc, OP_JUMP_U16);
    if (compiler->loop.break_count >= compiler->loop.break_capacity) {
        int old = compiler->loop.break_capacity;
        int new_cap = old < 8 ? 8 : old * 2;
        compiler->loop.break_jumps = (size_t*)grow_array_raw(
            compiler->loop.break_jumps, old, new_cap, sizeof(size_t));
        compiler->loop.break_capacity = new_cap;
    }
    compiler->loop.break_jumps[compiler->loop.break_count++] = bc->code_size;
    emit_u16(compiler, bc, 0);
    return COMPILE_SUCCESS;
}


CompileResult compile_continue(Compiler* compiler, Bytecode* bytecode) {
    if (compiler->loop.loop_depth <= 0) {
        compiler_error(compiler, "continue used outside of loop");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    emit_op(compiler, bc, OP_JUMP_U16);
    if (compiler->loop.continue_count >= compiler->loop.continue_capacity) {
        int old = compiler->loop.continue_capacity;
        int new_cap = old < 8 ? 8 : old * 2;
        compiler->loop.continue_jumps = (size_t*)grow_array_raw(
            compiler->loop.continue_jumps, old, new_cap, sizeof(size_t));
        compiler->loop.continue_capacity = new_cap;
    }
    compiler->loop.continue_jumps[compiler->loop.continue_count++] = bc->code_size;
    emit_u16(compiler, bc, 0);
    return COMPILE_SUCCESS;
}


/* операторы → opcode */
int get_operator_precedence(TokenType type) {
    switch (type) {
        case TOKEN_OR:
            return 1;
        case TOKEN_AND:
            return 2;
        case TOKEN_EQUAL_EQUAL:
        case TOKEN_BANG_EQUAL:
            return 3;

        case TOKEN_LESS:
        case TOKEN_LESS_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_GREATER_EQUAL:
            return 4;

        case TOKEN_PLUS:
        case TOKEN_MINUS:
            return 5;

        case TOKEN_STAR:
        case TOKEN_SLASH:
        case TOKEN_PERCENT:
            return 6;

        case TOKEN_BIT_LEFT_SHIFT:
        case TOKEN_BIT_RIGHT_SHIFT:
        case TOKEN_BIT_AND:
        case TOKEN_BIT_OR:
        case TOKEN_BIT_XOR:
            return 0;

        case TOKEN_EQUAL:
        case TOKEN_PLUS_EQUAL:
        case TOKEN_MINUS_EQUAL:
        case TOKEN_STAR_EQUAL:
        case TOKEN_SLASH_EQUAL:
        case TOKEN_PERCENT_EQUAL:
        case TOKEN_BIT_AND_EQUAL:
        case TOKEN_BIT_OR_EQUAL:
        case TOKEN_BIT_XOR_EQUAL:
        case TOKEN_BIT_LEFT_SHIFT_EQUAL:
        case TOKEN_BIT_RIGHT_SHIFT_EQUAL:
            return 0;

        default:
            return 0;
    }
}

OpCode get_binary_opcode(TokenType type) {
    switch (type) {
        case TOKEN_PLUS:          return OP_ADD;
        case TOKEN_MINUS:         return OP_SUB;
        case TOKEN_STAR:          return OP_MUL;
        case TOKEN_SLASH:         return OP_DIV;
        case TOKEN_PERCENT:       return OP_MOD;

            /* сравнения */
        case TOKEN_EQUAL_EQUAL:   return OP_EQ;
        case TOKEN_BANG_EQUAL:    return OP_NEQ;
        case TOKEN_LESS:          return OP_LT;
        case TOKEN_LESS_EQUAL:    return OP_LE;
        case TOKEN_GREATER:       return OP_GT;
        case TOKEN_GREATER_EQUAL: return OP_GE;

        case TOKEN_AND:
        case TOKEN_OR:
            return OP_NOP;

        case TOKEN_BIT_AND:
        case TOKEN_BIT_OR:
        case TOKEN_BIT_XOR:
        case TOKEN_BIT_LEFT_SHIFT:
        case TOKEN_BIT_RIGHT_SHIFT:
            return OP_NOP;

        default:
            return OP_NOP;
    }
}


OpCode get_unary_opcode(TokenType type) {
    switch (type) {
        case TOKEN_MINUS:
            return OP_NEG;
        case TOKEN_BANG:
            return OP_NOT;

        case TOKEN_BIT_NOT:
            return OP_NOP;

        default:
            return OP_NOP;
    }
}

/* Пока нет post‑fix опкодов (++, -- как отдельные инструкции),
   index/call реализуются не через get_postfix_opcode, а отдельными узлами AST. */
OpCode get_postfix_opcode(TokenType type) {
    (void)type;
    return OP_NOP;
}