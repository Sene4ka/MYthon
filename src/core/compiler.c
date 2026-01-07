#include "compiler.h"

#include <stdarg.h>

#include "memory.h"
#include "utils.h"
#include "native.h"

#include <string.h>
#include <stdio.h>

static void compiler_debug_log(Compiler* c, const char* fmt, ...) {
    if (!c || !c->debug) return;
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "[COMPILER] ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
}

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

    compiler->debug = 0;
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

    compiler_debug_log(compiler, "add_local(\"%.*s\") scope_depth=%d count=%d",
                       length, name ? name : "", fs->scope_depth, fs->locals.count);

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

    compiler_debug_log(compiler, "  add_local: idx=%d max_locals=%d",
                       idx, fs->max_locals);

    return idx;
}

int compiler_resolve_local(Compiler* compiler, const char* name, int length) {
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return -1;

    compiler_debug_log(compiler, "resolve_local(\"%.*s\") count=%d",
                       length, name ? name : "", fs->locals.count);

    LocalTable* locals = &fs->locals;
    for (int i = locals->count - 1; i >= 0; i--) {
        if ((int)strlen(locals->names[i]) == length &&
            memcmp(locals->names[i], name, length) == 0) {
            compiler_debug_log(compiler, "  resolve_local: found idx=%d", i);
            return i;
            }
    }
    compiler_debug_log(compiler, "  resolve_local: not found");
    return -1;
}

int compiler_resolve_upvalue(Compiler* compiler, const char* name, int length) {
    compiler_debug_log(compiler, "resolve_upvalue(\"%.*s\") depth=%d current_func=%d",
                       length, name ? name : "",
                       compiler->function_depth, compiler->current_function);

    if (compiler->current_function <= 0) {
        compiler_debug_log(compiler, "  upvalue: no outer function (current_function=%d)",
                           compiler->current_function);
        return -1;
    }

    int outer_index = compiler->current_function - 1;
    if (outer_index < 0 || outer_index >= compiler->function_depth) {
        compiler_debug_log(compiler, "  upvalue: outer_index out of range (%d)", outer_index);
        return -1;
    }

    Compiler outer_tmp = *compiler;
    outer_tmp.current_function = outer_index;
    FunctionState* outer_fs = &compiler->functions[outer_index];

    int local = compiler_resolve_local(&outer_tmp, name, length);
    if (local >= 0) {
        compiler_debug_log(compiler, "  upvalue: capture outer local idx=%d", local);

        outer_fs->locals.is_captured[local] = 1;

        FunctionState* fs = current_function_state(compiler);
        for (int i = 0; i < fs->upvalue_count; i++) {
            UpvalueEntry* uv = &fs->upvalues[i];
            if (uv->is_local && uv->index == local) {
                compiler_debug_log(compiler, "  upvalue: already have idx=%d", i);
                return i;
            }
        }
        ensure_upvalue_capacity(fs, fs->upvalue_count + 1);
        int idx = fs->upvalue_count++;
        fs->upvalues[idx].is_local = 1;
        fs->upvalues[idx].index = local;
        compiler_debug_log(compiler, "  upvalue: new local upvalue idx=%d", idx);
        return idx;
    }

    int up = compiler_resolve_upvalue(&outer_tmp, name, length);
    if (up >= 0) {
        compiler_debug_log(compiler, "  upvalue: capture outer upvalue idx=%d", up);

        FunctionState* fs = current_function_state(compiler);
        for (int i = 0; i < fs->upvalue_count; i++) {
            UpvalueEntry* uv = &fs->upvalues[i];
            if (!uv->is_local && uv->index == up) {
                compiler_debug_log(compiler, "  upvalue: already have idx=%d", i);
                return i;
            }
        }
        ensure_upvalue_capacity(fs, fs->upvalue_count + 1);
        int idx = fs->upvalue_count++;
        fs->upvalues[idx].is_local = 0;
        fs->upvalues[idx].index = up;
        compiler_debug_log(compiler, "  upvalue: new non-local upvalue idx=%d", idx);
        return idx;
    }

    compiler_debug_log(compiler, "  upvalue: not found");
    return -1;
}

void compiler_begin_scope(Compiler* compiler) {
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return;
    fs->scope_depth++;
}

void compiler_end_scope(Compiler* compiler, Bytecode* bytecode, int start_local_count) {
    (void)bytecode;
    FunctionState* fs = current_function_state(compiler);
    if (!fs) return;

    fs->scope_depth--;
    LocalTable* locals = &fs->locals;

    while (locals->count > start_local_count &&
           locals->depths[locals->count - 1] > fs->scope_depth) {
        locals->count--;
    }
}

void compiler_begin_loop(Compiler* compiler) {
    compiler->loop.loop_depth++;
}

void compiler_end_loop(Compiler* compiler, Bytecode* bytecode) {
    (void)bytecode;
    if (compiler->loop.loop_depth > 0) {
        compiler->loop.loop_depth--;
    }
}

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

void compiler_begin_function(Compiler* compiler,
                             Bytecode* bytecode,
                             const char* name,
                             int arity,
                             int line,
                             int* out_func_index) {
    (void)line;

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
        (uint16_t)0,
        (uint16_t)0
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

    uint32_t code_start = (uint32_t)bytecode->code_size;

    for (size_t i = 0; i < fbc->code_size; i++) {
        bc_write_u8(bytecode,
                    fbc->code[i],
                    fbc->line_numbers ? fbc->line_numbers[i]
                                      : compiler->current_line);
    }

    uint32_t code_end = (uint32_t)bytecode->code_size;
    fs->start_ip = (int)code_start;
    fs->end_ip   = (int)code_end;

    for (int i = 0; i < fs->upvalue_count; i++) {
        UpvalueEntry* uv = &fs->upvalues[i];
        bc_add_upvalue(bytecode,
                       (uint32_t)fs->func_index,
                       (uint16_t)uv->index,
                       uv->is_local ? true : false);
    }

    Function* fn = &bytecode->functions[fs->func_index];
    fn->code_start = code_start;
    fn->code_end   = code_end;
    fn->max_locals = (uint16_t)fs->max_locals;

    bc_end_function(bytecode, (uint32_t)fs->func_index);

    bytecode_free(fbc);
    fs->func_bc = NULL;

    compiler->function_depth--;
    compiler->current_function = compiler->function_depth - 1;
}

int compiler_resolve_global(Compiler* compiler, Bytecode* bytecode, const char* name) {
    compiler_debug_log(compiler, "resolve_global(\"%s\")", name ? name : "<null>");

    int raw = bc_resolve_global(bytecode, name);
    compiler_debug_log(compiler, "  resolve_global: raw=%d native_offset=%d",
                       raw, compiler->native_global_offset);

    if (raw < 0) return -1;
    return compiler->native_global_offset + raw;
}

int compiler_define_global(Compiler* compiler, Bytecode* bytecode, const char* name, int const_idx) {
    compiler_debug_log(compiler, "define_global(\"%s\", const_idx=%d)",
                       name ? name : "<null>", const_idx);

    int raw = bc_define_global(bytecode, name, const_idx);
    compiler_debug_log(compiler, "  define_global: raw=%d native_offset=%d",
                       raw, compiler->native_global_offset);

    if (raw < 0) return -1;
    return compiler->native_global_offset + raw;
}

CompileResult compiler_compile(Compiler* compiler, const ASTNode* ast, Bytecode* bytecode) {
    compiler_debug_log(compiler, "compile start root");
    compiler->error_count = 0;

    int main_index = -1;
    compiler_begin_function(compiler, bytecode, NULL, 0, 1, &main_index);

    compiler_debug_log(compiler, "before compile_program");
    CompileResult result = compile_program(compiler, ast, bytecode);
    compiler_debug_log(compiler, "after compile_program result=%d errors=%d",
                       result, compiler->error_count);

    if (compiler->error_count == 0) {
        Bytecode* bc = current_bc(compiler, bytecode);
        emit_op(compiler, bc, OP_RETURN_NIL);
        compiler_end_function(compiler, bytecode);
        compiler_debug_log(compiler, "DEBUG: end root OK\n");
        return result;
    }

    compiler_debug_log(compiler, "DEBUG: end root with errors\n");
    return COMPILE_ERROR;
}

CompileResult compile_expression(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    compiler_debug_log(compiler, "compile_expression: type=%d line=%d",
                       node ? (int)node->type : -1,
                       node ? (int)node->line : -1);
    return compile_node(compiler, node, bytecode);
}

CompileResult compile_node(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node) {
        compiler_debug_log(compiler, "compile_node: NULL node, skip");
        return COMPILE_SUCCESS;
    }

    compiler_debug_log(compiler, "compile_node: type=%d line=%d", node->type, node->line);

    switch (node->type) {
        case NODE_LITERAL_EXPR:
            compiler_debug_log(compiler, "  -> literal");
            return compile_literal(compiler, node, bytecode);

        case NODE_VARIABLE_EXPR:
            compiler_debug_log(compiler, "  -> variable");
            return compile_variable(compiler, node, bytecode);

        case NODE_UNARY_EXPR:
            compiler_debug_log(compiler, "  -> unary");
            return compile_unary(compiler, node, bytecode);

        case NODE_BINARY_EXPR:
            compiler_debug_log(compiler, "  -> binary");
            return compile_binary(compiler, node, bytecode);

        case NODE_LOGICAL_EXPR:
            compiler_debug_log(compiler, "  -> logical");
            return compile_logical(compiler, node, bytecode);

        case NODE_ASSIGN_EXPR:
            compiler_debug_log(compiler, "  -> assign");
            return compile_assign(compiler, node, bytecode);

        case NODE_CALL_EXPR:
            compiler_debug_log(compiler, "  -> call");
            return compile_call(compiler, node, bytecode);

        case NODE_ARRAY_EXPR:
            compiler_debug_log(compiler, "  -> array");
            return compile_array(compiler, node, bytecode);

        case NODE_INDEX_EXPR:
            compiler_debug_log(compiler, "  -> index");
            return compile_index(compiler, node, bytecode);

        case NODE_TERNARY_EXPR:
            compiler_debug_log(compiler, "  -> ternary");
            return compile_ternary(compiler, node, bytecode);

        case NODE_GROUP_EXPR:
            compiler_debug_log(compiler, "  -> group");
            return compile_expression(compiler, node->unary.operand, bytecode);

        case NODE_MEMBER_EXPR:
            compiler_debug_log(compiler, "  -> member");
            return compile_member(compiler, node, bytecode);

        case NODE_EXPR_STMT:
        case NODE_BLOCK_STMT:
        case NODE_IF_STMT:
        case NODE_FOR_STMT:
        case NODE_RETURN_STMT:
        case NODE_FUNCTION_STMT:
        case NODE_BREAK_STMT:
        case NODE_CONTINUE_STMT:
            compiler_debug_log(compiler, "  -> statement");
            return compile_statement(compiler, node, bytecode);

        case NODE_PROGRAM:
            compiler_debug_log(compiler, "  -> program");
            return compile_program(compiler, node, bytecode);

        default:
            compiler_debug_log(compiler, "  -> unsupported node type=%d", node->type);
            compiler_error_at_line(compiler, node->line, "Unsupported node type (yet)");
            return COMPILE_ERROR;
    }
}


CompileResult compile_statement(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node) {
        compiler_debug_log(compiler, "compile_statement: NULL node, skip");
        return COMPILE_SUCCESS;
    }

    compiler_debug_log(compiler, "compile_statement: type=%d line=%d", node->type, node->line);

    switch (node->type) {
        case NODE_EXPR_STMT: {
            compiler_debug_log(compiler, "  -> expr stmt");
            Bytecode* bc = current_bc(compiler, bytecode);
            CompileResult r = compile_expression(compiler, node->expr_stmt.expression, bytecode);
            if (r != COMPILE_SUCCESS) {
                compiler_debug_log(compiler, "  expr stmt FAILED line=%d", node->line);
                return r;
            }
            emit_op(compiler, bc, OP_POP);
            return COMPILE_SUCCESS;
        }

        case NODE_BLOCK_STMT:
            compiler_debug_log(compiler, "  -> block stmt");
            return compile_block(compiler, node, bytecode);

        case NODE_IF_STMT:
            compiler_debug_log(compiler, "  -> if stmt");
            return compile_if(compiler, node, bytecode);

        case NODE_FOR_STMT:
            compiler_debug_log(compiler, "  -> for stmt");
            return compile_for(compiler, node, bytecode);

        case NODE_RETURN_STMT:
            compiler_debug_log(compiler, "  -> return stmt");
            return compile_return(compiler, node, bytecode);

        case NODE_FUNCTION_STMT:
            compiler_debug_log(compiler, "  -> function stmt");
            return compile_function(compiler, node, bytecode);

        case NODE_BREAK_STMT:
            compiler_debug_log(compiler, "  -> break stmt");
            return compile_break(compiler, bytecode);

        case NODE_CONTINUE_STMT:
            compiler_debug_log(compiler, "  -> continue stmt");
            return compile_continue(compiler, bytecode);

        default:
            compiler_debug_log(compiler, "  -> unsupported statement type=%d", node->type);
            compiler_error_at_line(compiler, node->line, "Unsupported statement type");
            return COMPILE_ERROR;
    }
}


CompileResult compile_program(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_PROGRAM) {
        compiler_error(compiler, "Expected program node");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "program start");
    ASTNode* stmt = node->program.statements;
    while (stmt) {
        compiler_debug_log(compiler, "stmt type=%d line=%d", stmt->type, stmt->line);

        CompileResult r = compile_node(compiler, stmt, bytecode);
        if (r != COMPILE_SUCCESS) return r;
        stmt = stmt->next;
    }
    compiler_debug_log(compiler, "program end");
    return COMPILE_SUCCESS;
}

CompileResult compile_block(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_BLOCK_STMT) {
        compiler_error(compiler, "Expected block statement");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_block: line=%d", node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    FunctionState* fs = current_function_state(compiler);
    int start_locals = fs ? fs->locals.count : 0;

    compiler_begin_scope(compiler);
    compiler_debug_log(compiler, "  block: begin scope, start_locals=%d", start_locals);

    ASTNode* stmt = node->block.statements;
    while (stmt) {
        compiler_debug_log(compiler, "  block: stmt type=%d line=%d", stmt->type, stmt->line);
        CompileResult r = compile_statement(compiler, stmt, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  block: stmt FAILED line=%d", stmt->line);
            compiler_end_scope(compiler, bc, start_locals);
            return r;
        }
        stmt = stmt->next;
    }

    compiler_end_scope(compiler, bc, start_locals);
    compiler_debug_log(compiler, "  block: end scope");
    return COMPILE_SUCCESS;
}

CompileResult compile_unary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_UNARY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected unary expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_unary: op=%d line=%d",
                       node->unary.operator, node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    CompileResult r = compile_expression(compiler, node->unary.operand, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  unary: operand FAILED line=%d", node->line);
        return r;
    }

    OpCode op = get_unary_opcode(node->unary.operator);
    if (op == OP_NOP) {
        compiler_error_at_line(compiler, node->line, "Unsupported unary operator");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "  unary: emit op=%d", op);
    emit_op(compiler, bc, op);
    return COMPILE_SUCCESS;
}


CompileResult compile_binary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_BINARY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected binary expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_binary: op=%d line=%d",
                       node->binary.operator, node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    compiler_debug_log(compiler, "  binary: left");
    CompileResult r = compile_expression(compiler, node->binary.left, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  binary: left FAILED line=%d", node->line);
        return r;
    }

    compiler_debug_log(compiler, "  binary: right");
    r = compile_expression(compiler, node->binary.right, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  binary: right FAILED line=%d", node->line);
        return r;
    }

    OpCode op = get_binary_opcode(node->binary.operator);
    if (op == OP_NOP) {
        compiler_error_at_line(compiler, node->line, "Unsupported binary operator");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "  binary: emit op=%d", op);
    emit_op(compiler, bc, op);
    return COMPILE_SUCCESS;
}

CompileResult compile_postfix(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    (void)bytecode;

    compiler_debug_log(compiler, "compile_postfix: UNUSED helper called, line=%d type=%d",
                       node ? (int)node->line : -1,
                       node ? (int)node->type : -1);

    return COMPILE_ERROR;
}

CompileResult compile_literal(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_LITERAL_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected literal expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_literal: line=%d kind=%d",
                       node->line, node->literal.value_type);

    Bytecode* bc = current_bc(compiler, bytecode);
    int const_idx = -1;

    switch (node->literal.value_type) {
        case LIT_NUMBER: {
            double value = node->literal.value.number;
            int64_t iv = (int64_t)value;
            compiler_debug_log(compiler, "  literal number=%g", value);
            if ((double)iv == value) {
                const_idx = compiler_add_const_int(bytecode, iv);
            } else {
                const_idx = compiler_add_const_float(bytecode, value);
            }
            break;
        }
        case LIT_STRING: {
            const char* s = node->literal.value.string;
            compiler_debug_log(compiler, "  literal string=\"%s\"", s ? s : "");
            const_idx = compiler_add_const_string(bytecode, s ? s : "");
            break;
        }
        case LIT_BOOL: {
            compiler_debug_log(compiler, "  literal bool=%d", node->literal.value.boolean ? 1 : 0);
            const_idx = compiler_add_const_int(bytecode,
                                               node->literal.value.boolean ? 1 : 0);
            break;
        }
        case LIT_NIL: {
            compiler_debug_log(compiler, "  literal nil");
            const_idx = compiler_add_const_int(bytecode, 0);
            break;
        }
        default:
            compiler_debug_log(compiler, "  literal: unknown type=%d", node->literal.value_type);
            compiler_error_at_line(compiler, node->line, "Unknown literal type");
            return COMPILE_ERROR;
    }

    if (const_idx < 0) {
        compiler_debug_log(compiler, "  literal: failed to add const");
        compiler_error_at_line(compiler, node->line, "Failed to add constant");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "  literal: const_idx=%d", const_idx);
    emit_op(compiler, bc, OP_LOAD_CONST_U16);
    emit_u16(compiler, bc, (uint16_t)const_idx);
    return COMPILE_SUCCESS;
}

CompileResult compile_variable(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_VARIABLE_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected variable expression");
        return COMPILE_ERROR;
    }

    const char* name = node->variable.name;
    int length = (int)node->variable.name_length;

    compiler_debug_log(compiler, "compile_variable: \"%.*s\" line=%d",
                       length, name, node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    int local = compiler_resolve_local(compiler, name, length);
    if (local >= 0) {
        compiler_debug_log(compiler, "  -> local index=%d", local);
        if (local <= 0xFF) {
            emit_op(compiler, bc, OP_LOAD_LOCAL_U8);
            emit_u8(compiler, bc, (uint8_t)local);
        } else {
            compiler_error_at_line(compiler, node->line, "Too many locals");
            return COMPILE_ERROR;
        }
        return COMPILE_SUCCESS;
    }

    int up = compiler_resolve_upvalue(compiler, name, length);
    if (up >= 0) {
        compiler_debug_log(compiler, "  -> upvalue index=%d", up);
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
        compiler_debug_log(compiler, "  -> native \"%s\" global_index=%d", name, global_index);
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
        compiler_debug_log(compiler, "  -> global index=%d", global);
        emit_op(compiler, bc, OP_LOAD_GLOBAL_U16);
        emit_u16(compiler, bc, (uint16_t)global);
        return COMPILE_SUCCESS;
    }

    compiler_debug_log(compiler, "  -> unknown variable");
    compiler_error_at_line(compiler, node->line, "Unknown variable");
    return COMPILE_ERROR;
}

CompileResult compile_assign(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_ASSIGN_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected assignment expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_assign: line=%d", node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    const ASTNode* target = node->assign.target;
    const ASTNode* value  = node->assign.value;

    compiler_debug_log(compiler, "  assign: target_type=%d", target ? (int)target->type : -1);

    CompileResult r = compile_expression(compiler, value, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  assign: value FAILED");
        return r;
    }

    if (target->type == NODE_VARIABLE_EXPR) {
        const char* name = target->variable.name;
        int length = (int)target->variable.name_length;

        compiler_debug_log(compiler, "  assign to variable \"%.*s\"", length, name);

        int local = compiler_resolve_local(compiler, name, length);
        if (local >= 0) {
            compiler_debug_log(compiler, "    -> local index=%d", local);
            if (local > 0xFF) {
                compiler_error_at_line(compiler, node->line, "Too many locals");
                return COMPILE_ERROR;
            }
            emit_op(compiler, bc, OP_STORE_LOCAL_U8);
            emit_u8(compiler, bc, (uint8_t)local);
            return COMPILE_SUCCESS;
        }

        int up = compiler_resolve_upvalue(compiler, name, length);
        if (up >= 0) {
            compiler_debug_log(compiler, "    -> upvalue index=%d", up);
            if (up > 0xFF) {
                compiler_error_at_line(compiler, node->line, "Too many upvalues");
                return COMPILE_ERROR;
            }
            emit_op(compiler, bc, OP_STORE_UPVALUE_U8);
            emit_u8(compiler, bc, (uint8_t)up);
            return COMPILE_SUCCESS;
        }

        char buf[256];
        int name_len = length < (int)sizeof(buf)-1 ? length : (int)sizeof(buf)-1;
        memcpy(buf, name, name_len);
        buf[name_len] = '\0';

        compiler_debug_log(compiler, "    -> global lookup \"%s\"", buf);

        int global = compiler_resolve_global(compiler, bytecode, buf);
        if (global < 0) {
            compiler_debug_log(compiler, "    -> define new global \"%s\"", buf);
            int const_idx = -1;
            global = compiler_define_global(compiler, bytecode, buf, const_idx);
            if (global < 0) {
                compiler_error_at_line(compiler, node->line, "Failed to define global");
                return COMPILE_ERROR;
            }
        } else {
            compiler_debug_log(compiler, "    -> existing global index=%d", global);
        }

        compiler_debug_log(compiler, "    -> emit STORE_GLOBAL index=%d", global);

        emit_op(compiler, bc, OP_STORE_GLOBAL_U16);
        emit_u16(compiler, bc, (uint16_t)global);
        return COMPILE_SUCCESS;
    }

    if (target->type == NODE_INDEX_EXPR) {
        compiler_debug_log(compiler, "  assign to index expr");

        emit_op(compiler, bc, OP_POP);
        compiler_debug_log(compiler, "    popped original value to rebuild arr,index,value");

        r = compile_expression(compiler, target->index.array, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "    array expr FAILED");
            return r;
        }

        r = compile_expression(compiler, target->index.index, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "    index expr FAILED");
            return r;
        }

        r = compile_expression(compiler, value, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "    value expr FAILED (rebuild)");
            return r;
        }

        compiler_debug_log(compiler, "    emit OP_ARRAY_SET");
        emit_op(compiler, bc, OP_ARRAY_SET);
        return COMPILE_SUCCESS;
    }

    compiler_debug_log(compiler, "  assign: invalid target_type=%d", target->type);
    compiler_error_at_line(compiler, node->line, "Invalid assignment target");
    return COMPILE_ERROR;
}

CompileResult compile_call(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_CALL_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected call expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_call: line=%d argc=%zu",
                       node->line, node->call.arg_count);

    Bytecode* bc = current_bc(compiler, bytecode);

    const ASTNode* callee = node->call.callee;

    compiler_debug_log(compiler, "  call: callee type=%d", callee ? (int)callee->type : -1);
    CompileResult r = compile_expression(compiler, callee, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  call: callee FAILED");
        return r;
    }

    for (size_t i = 0; i < node->call.arg_count; i++) {
        compiler_debug_log(compiler, "  call: arg[%zu] type=%d",
                           i, node->call.arguments[i]->type);
        r = compile_expression(compiler, node->call.arguments[i], bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  call: arg[%zu] FAILED", i);
            return r;
        }
    }

    size_t argc = node->call.arg_count;
    compiler_debug_log(compiler, "  call: emit CALL argc=%zu", argc);

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

    compiler_debug_log(compiler, "compile_logical: line=%d op=%d",
                       node->line, node->logical.operator);

    Bytecode* bc = current_bc(compiler, bytecode);
    TokenType op = node->logical.operator;

    compiler_debug_log(compiler, "  logical: left");
    CompileResult r = compile_expression(compiler, node->logical.left, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  logical: left FAILED");
        return r;
    }

    size_t jump_pos = 0;

    if (op == TOKEN_OR) {
        compiler_debug_log(compiler, "  logical: emit JUMP_IF_TRUE");
        emit_jump_u16(compiler, bc, OP_JUMP_IF_TRUE_U16, &jump_pos);
    } else {
        compiler_debug_log(compiler, "  logical: emit JUMP_IF_FALSE");
        emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &jump_pos);
    }

    compiler_debug_log(compiler, "  logical: right");
    r = compile_expression(compiler, node->logical.right, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  logical: right FAILED");
        return r;
    }

    compiler_debug_log(compiler, "  logical: patch jump at %zu", jump_pos);
    patch_jump_u16(bc, jump_pos);
    return COMPILE_SUCCESS;
}

CompileResult compile_array(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_ARRAY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected array expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_array: line=%d count=%zu",
                       node->line, node->array.element_count);

    Bytecode* bc = current_bc(compiler, bytecode);

    for (size_t i = 0; i < node->array.element_count; i++) {
        compiler_debug_log(compiler, "  array: elem[%zu] type=%d",
                           i, node->array.elements[i]->type);
        CompileResult r = compile_expression(compiler, node->array.elements[i], bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  array: elem[%zu] FAILED", i);
            return r;
        }
    }

    if (node->array.element_count > 0xFF) {
        compiler_debug_log(compiler, "  array: too many elements=%zu",
                           node->array.element_count);
        compiler_error_at_line(compiler, node->line, "Too many array elements");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "  array: emit ARRAY_NEW %zu", node->array.element_count);
    emit_op(compiler, bc, OP_ARRAY_NEW_U8);
    emit_u8(compiler, bc, (uint8_t)node->array.element_count);
    return COMPILE_SUCCESS;
}

CompileResult compile_index(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_INDEX_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected index expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_index: line=%d", node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    compiler_debug_log(compiler, "  index: array expr");
    CompileResult r = compile_expression(compiler, node->index.array, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  index: array FAILED");
        return r;
    }

    compiler_debug_log(compiler, "  index: index expr");
    r = compile_expression(compiler, node->index.index, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  index: index FAILED");
        return r;
    }

    compiler_debug_log(compiler, "  index: emit OP_ARRAY_GET");
    emit_op(compiler, bc, OP_ARRAY_GET);
    return COMPILE_SUCCESS;
}


CompileResult compile_member(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    (void)bytecode;

    compiler_debug_log(compiler, "compile_member: line=%d (NOT IMPLEMENTED)",
                       node ? node->line : -1);

    compiler_error_at_line(compiler, node ? node->line : 0,
                           "Member expressions not yet implemented");
    return COMPILE_ERROR;
}


CompileResult compile_ternary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_TERNARY_EXPR) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected ternary expression");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_ternary: line=%d", node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    compiler_debug_log(compiler, "  ternary: condition");
    CompileResult r = compile_expression(compiler, node->ternary.condition, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  ternary: condition FAILED");
        return r;
    }

    size_t else_jump = 0;
    compiler_debug_log(compiler, "  ternary: emit JUMP_IF_FALSE");
    emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &else_jump);

    compiler_debug_log(compiler, "  ternary: then");
    r = compile_expression(compiler, node->ternary.then_expr, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  ternary: then FAILED");
        return r;
    }

    size_t end_jump = 0;
    compiler_debug_log(compiler, "  ternary: emit JUMP to end");
    emit_jump_u16(compiler, bc, OP_JUMP_U16, &end_jump);

    compiler_debug_log(compiler, "  ternary: patch else_jump at %zu", else_jump);
    patch_jump_u16(bc, else_jump);

    compiler_debug_log(compiler, "  ternary: else");
    r = compile_expression(compiler, node->ternary.else_expr, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  ternary: else FAILED");
        return r;
    }

    compiler_debug_log(compiler, "  ternary: patch end_jump at %zu", end_jump);
    patch_jump_u16(bc, end_jump);
    return COMPILE_SUCCESS;
}

CompileResult compile_if(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_IF_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected if statement");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_if: line=%d", node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    compiler_debug_log(compiler, "  if: condition");
    CompileResult r = compile_expression(compiler, node->if_stmt.condition, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  if: condition FAILED");
        return r;
    }

    size_t else_jump = 0;
    compiler_debug_log(compiler, "  if: emit JUMP_IF_FALSE");
    emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &else_jump);

    compiler_debug_log(compiler, "  if: THEN branch");
    r = compile_statement(compiler, node->if_stmt.then_branch, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  if: THEN branch FAILED");
        return r;
    }

    size_t end_jump = 0;
    if (node->if_stmt.else_branch) {
        compiler_debug_log(compiler, "  if: emit JUMP to end (has else)");
        emit_jump_u16(compiler, bc, OP_JUMP_U16, &end_jump);
    }

    compiler_debug_log(compiler, "  if: patch else_jump at %zu", else_jump);
    patch_jump_u16(bc, else_jump);

    if (node->if_stmt.else_branch) {
        compiler_debug_log(compiler, "  if: ELSE POP cond");

        compiler_debug_log(compiler, "  if: ELSE branch");
        r = compile_statement(compiler, node->if_stmt.else_branch, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  if: ELSE branch FAILED");
            return r;
        }

        compiler_debug_log(compiler, "  if: patch end_jump at %zu", end_jump);
        patch_jump_u16(bc, end_jump);
    } else {
        compiler_debug_log(compiler, "  if: no ELSE, POP cond in false branch");
    }

    compiler_debug_log(compiler, "compile_if: done line=%d", node->line);
    return COMPILE_SUCCESS;
}

CompileResult compile_for(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_FOR_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected for statement");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_for: start line=%d", node->line);

    Bytecode* bc = current_bc(compiler, bytecode);

    compiler_begin_scope(compiler);
    compiler_debug_log(compiler, "  for: begin scope");

    if (node->for_stmt.initializer) {
        compiler_debug_log(compiler, "  for: init expr type=%d",
                           node->for_stmt.initializer->type);
        CompileResult r = compile_expression(compiler, node->for_stmt.initializer, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  for: init FAILED");
            compiler_end_scope(compiler, bc,
                               current_function_state(compiler)->locals.count);
            return r;
        }
        emit_op(compiler, bc, OP_POP);
    }

    size_t loop_start = bc->code_size;
    compiler_debug_log(compiler, "  for: loop_start=%zu", loop_start);

    size_t exit_jump = 0;
    if (node->for_stmt.condition) {
        compiler_debug_log(compiler, "  for: condition expr type=%d",
                           node->for_stmt.condition->type);
        CompileResult r = compile_expression(compiler, node->for_stmt.condition, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  for: condition FAILED");
            compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);
            return r;
        }
        compiler_debug_log(compiler, "  for: emit JUMP_IF_FALSE (exit)");
        emit_jump_u16(compiler, bc, OP_JUMP_IF_FALSE_U16, &exit_jump);
    }

    compiler_begin_loop(compiler);
    compiler_debug_log(compiler, "  for: begin loop body");

    CompileResult r = compile_statement(compiler, node->for_stmt.body, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  for: body FAILED");
        compiler_end_loop(compiler, bc);
        compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);
        return r;
    }

    if (node->for_stmt.increment) {
        compiler_debug_log(compiler, "  for: increment expr type=%d",
                           node->for_stmt.increment->type);
        r = compile_expression(compiler, node->for_stmt.increment, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  for: increment FAILED");
            compiler_end_loop(compiler, bc);
            compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);
            return r;
        }
        emit_op(compiler, bc, OP_POP);
    }

    compiler_debug_log(compiler, "  for: emit loop back to %zu", loop_start);
    emit_loop_u16(compiler, bc, loop_start);

    size_t end_ip = bc->code_size;
    compiler_debug_log(compiler, "  for: patch continues to %zu", loop_start);
    compiler_patch_continues(compiler, bc, loop_start);
    compiler_debug_log(compiler, "  for: patch breaks to %zu", end_ip);
    compiler_patch_breaks(compiler, bc, end_ip);

    if (node->for_stmt.condition) {
        compiler_debug_log(compiler, "  for: patch exit_jump at %zu", exit_jump);
        patch_jump_u16(bc, exit_jump);
    }

    compiler_end_loop(compiler, bc);
    compiler_end_scope(compiler, bc, current_function_state(compiler)->locals.count);

    compiler_debug_log(compiler, "compile_for: end line=%d", node->line);
    return COMPILE_SUCCESS;
}

CompileResult compile_function(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_FUNCTION_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected function statement");
        return COMPILE_ERROR;
    }

    const char* name = node->function_stmt.name;
    size_t name_len  = node->function_stmt.name_length;

    compiler_debug_log(compiler, "compile_function: \"%.*s\" line=%d params=%zu",
                       (int)name_len, name ? name : "",
                       node->line, node->function_stmt.param_count);

    int local_index = -1;
    if (name && name_len > 0) {
        local_index = compiler_resolve_local(compiler, name, (int)name_len);
        if (local_index < 0) {
            compiler_debug_log(compiler, "  function: add local name");
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
        compiler_debug_log(compiler, "  function: local_index=%d", local_index);
    }

    int func_index = -1;
    compiler_debug_log(compiler, "  function: begin function \"%.*s\"",
                       (int)name_len, name ? name : "");
    compiler_begin_function(compiler,
                            bytecode,
                            name ? name : "<fn>",
                            (int)node->function_stmt.param_count,
                            node->line,
                            &func_index);

    FunctionState* fs = current_function_state(compiler);
    compiler_debug_log(compiler, "  function: func_index=%d", func_index);

    for (size_t i = 0; i < node->function_stmt.param_count; i++) {
        const char* param_name = node->function_stmt.parameters[i];
        int param_len = (int)strlen(param_name);
        compiler_debug_log(compiler, "  function: add param[%zu] \"%.*s\"",
                           i, param_len, param_name);
        int idx = compiler_add_local(compiler, param_name, param_len);
        if (idx < 0) {
            compiler_error_at_line(compiler, node->line, "Failed to add function parameter");
            return COMPILE_ERROR;
        }
    }

    compiler_debug_log(compiler, "  function: body block");
    CompileResult r = compile_block(compiler, node->function_stmt.body, bytecode);
    if (r != COMPILE_SUCCESS) {
        compiler_debug_log(compiler, "  function: body FAILED");
        return r;
    }

    Bytecode* fbc = current_bc(compiler, bytecode);
    compiler_debug_log(compiler, "  function: emit implicit RETURN_NIL");
    emit_op(compiler, fbc, OP_RETURN_NIL);

    compiler_debug_log(compiler, "  function: end function");
    compiler_end_function(compiler, bytecode);

    Bytecode* bc = current_bc(compiler, bytecode);

    uint16_t upcount = (uint16_t)fs->upvalue_count;
    compiler_debug_log(compiler, "  function: upvalues=%u", (unsigned)upcount);
    int closure_const = bc_add_closure(bytecode, (uint32_t)func_index, upcount);
    if (closure_const < 0) {
        compiler_error_at_line(compiler, node->line, "Failed to add closure constant");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "  function: emit LOAD_CONST closure_const=%d", closure_const);
    emit_op(compiler, bc, OP_LOAD_CONST_U16);
    emit_u16(compiler, bc, (uint16_t)closure_const);

    compiler_debug_log(compiler, "  function: emit NEW_CLOSURE");
    emit_op(compiler, bc, OP_NEW_CLOSURE);

    if (local_index >= 0) {
        compiler_debug_log(compiler, "  function: store closure to local %d", local_index);
        emit_op(compiler, bc, OP_STORE_LOCAL_U8);
        emit_u8(compiler, bc, (uint8_t)local_index);
    }

    compiler_debug_log(compiler, "compile_function: end \"%.*s\"",
                       (int)name_len, name ? name : "");
    return COMPILE_SUCCESS;
}

CompileResult compile_return(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node || node->type != NODE_RETURN_STMT) {
        compiler_error_at_line(compiler, node ? node->line : 0, "Expected return statement");
        return COMPILE_ERROR;
    }

    compiler_debug_log(compiler, "compile_return: line=%d has_value=%d",
                       node->line, node->return_stmt.value != NULL);

    Bytecode* bc = current_bc(compiler, bytecode);

    if (node->return_stmt.value) {
        CompileResult r = compile_expression(compiler, node->return_stmt.value, bytecode);
        if (r != COMPILE_SUCCESS) {
            compiler_debug_log(compiler, "  return: value FAILED");
            return r;
        }
        compiler_debug_log(compiler, "  return: emit OP_RETURN");
        emit_op(compiler, bc, OP_RETURN);
    } else {
        compiler_debug_log(compiler, "  return: emit OP_RETURN_NIL");
        emit_op(compiler, bc, OP_RETURN_NIL);
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_break(Compiler* compiler, Bytecode* bytecode) {
    compiler_debug_log(compiler, "compile_break");

    if (compiler->loop.loop_depth <= 0) {
        compiler_error(compiler, "break used outside of loop");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    emit_op(compiler, bc, OP_JUMP_U16);
    if (compiler->loop.break_count >= compiler->loop.break_capacity) {
        int old = compiler->loop.break_capacity;
        int new_cap = old < 8 ? 8 : old * 2;
        compiler_debug_log(compiler, "  break: grow break_jumps from %d to %d", old, new_cap);
        compiler->loop.break_jumps = (size_t*)grow_array_raw(
            compiler->loop.break_jumps, old, new_cap, sizeof(size_t));
        compiler->loop.break_capacity = new_cap;
    }
    compiler_debug_log(compiler, "  break: record jump at %zu", bc->code_size);
    compiler->loop.break_jumps[compiler->loop.break_count++] = bc->code_size;
    emit_u16(compiler, bc, 0);
    return COMPILE_SUCCESS;
}

CompileResult compile_continue(Compiler* compiler, Bytecode* bytecode) {
    compiler_debug_log(compiler, "compile_continue");

    if (compiler->loop.loop_depth <= 0) {
        compiler_error(compiler, "continue used outside of loop");
        return COMPILE_ERROR;
    }

    Bytecode* bc = current_bc(compiler, bytecode);

    emit_op(compiler, bc, OP_JUMP_U16);
    if (compiler->loop.continue_count >= compiler->loop.continue_capacity) {
        int old = compiler->loop.continue_capacity;
        int new_cap = old < 8 ? 8 : old * 2;
        compiler_debug_log(compiler, "  continue: grow continue_jumps from %d to %d", old, new_cap);
        compiler->loop.continue_jumps = (size_t*)grow_array_raw(
            compiler->loop.continue_jumps, old, new_cap, sizeof(size_t));
        compiler->loop.continue_capacity = new_cap;
    }
    compiler_debug_log(compiler, "  continue: record jump at %zu", bc->code_size);
    compiler->loop.continue_jumps[compiler->loop.continue_count++] = bc->code_size;
    emit_u16(compiler, bc, 0);
    return COMPILE_SUCCESS;
}

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

OpCode get_postfix_opcode(TokenType type) {
    (void)type;
    return OP_NOP;
}