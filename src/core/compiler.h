#ifndef MYTHON_COMPILER_H
#define MYTHON_COMPILER_H

#include "ast.h"
#include "bytecode.h"
#include "lexer.h"

typedef enum {
    COMPILE_SUCCESS,
    COMPILE_ERROR,
    COMPILE_SYNTAX_ERROR
} CompileResult;

typedef struct {
    char** names;
    int* depths;
    int* is_captured;
    int count;
    int capacity;
} LocalTable;

typedef struct {
    int is_local;
    int index;
} UpvalueEntry;

typedef struct {
    char* name;
    int arity;
    int func_index;

    int start_ip;
    int end_ip;

    int max_locals;
    int scope_depth;

    LocalTable locals;
    UpvalueEntry* upvalues;
    int upvalue_count;
    int upvalue_capacity;

    Bytecode* func_bc;
} FunctionState;

typedef struct {
    int loop_depth;
    size_t* break_jumps;
    int break_count;
    int break_capacity;
    size_t* continue_jumps;
    int continue_count;
    int continue_capacity;
} LoopState;

typedef struct {
    FunctionState* functions;
    int function_depth;
    int function_capacity;
    int current_function;

    LoopState loop;

    int error_count;
    const char* error_message;
    const char* source_file;
    int current_line;

    int locals_at_toplevel;
    int native_global_offset;

    int debug;
} Compiler;

void compiler_init(Compiler* compiler, const char* source_file);
void compiler_free(Compiler* compiler);

CompileResult compiler_compile(Compiler* compiler, const ASTNode* ast, Bytecode* bytecode);

CompileResult compile_node(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_program(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_block(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_expression(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_statement(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);

CompileResult compile_binary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_unary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_postfix(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_literal(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_variable(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_assign(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_call(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_logical(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_array(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_index(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_member(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_ternary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_if(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_for(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_function(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_return(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_break(Compiler* compiler, Bytecode* bytecode);
CompileResult compile_continue(Compiler* compiler, Bytecode* bytecode);

int compiler_resolve_local(Compiler* compiler, const char* name, int length);

int compiler_add_local(Compiler* compiler, const char* name, int length);

int compiler_resolve_upvalue(Compiler* compiler, const char* name, int length);

int compiler_resolve_global(Compiler* compiler, Bytecode* bytecode, const char* name);
int compiler_define_global(Compiler* compiler, Bytecode* bytecode, const char* name, int const_idx);

void compiler_begin_scope(Compiler* compiler);
void compiler_end_scope(Compiler* compiler, Bytecode* bytecode, int start_local_count);

void compiler_begin_loop(Compiler* compiler);
void compiler_end_loop(Compiler* compiler, Bytecode* bytecode);

void compiler_patch_breaks(Compiler* compiler, Bytecode* bytecode, size_t target_ip);
void compiler_patch_continues(Compiler* compiler, Bytecode* bytecode, size_t target_ip);

void compiler_begin_function(Compiler* compiler,
                             Bytecode* bytecode,
                             const char* name,
                             int arity,
                             int line,
                             int* out_func_index);

void compiler_end_function(Compiler* compiler, Bytecode* bytecode);

void emit_byte(Compiler* compiler, Bytecode* bytecode, uint8_t byte);
void emit_op(Compiler* compiler, Bytecode* bytecode, OpCode op);
void emit_u8(Compiler* compiler, Bytecode* bytecode, uint8_t value);
void emit_u16(Compiler* compiler, Bytecode* bytecode, uint16_t value);

int compiler_add_const_int(Bytecode* bytecode, int64_t value);
int compiler_add_const_float(Bytecode* bytecode, double value);
int compiler_add_const_string(Bytecode* bytecode, const char* value);

void emit_jump_u16(Compiler* compiler, Bytecode* bytecode,
                   OpCode op, size_t* patch_pos);

void patch_jump_u16(Bytecode* bytecode, size_t patch_pos);

void emit_loop_u16(Compiler* compiler, Bytecode* bytecode, size_t loop_start);

void compiler_error(Compiler* compiler, const char* message);
void compiler_error_at_line(Compiler* compiler, int line, const char* message);

OpCode get_binary_opcode(TokenType type);
OpCode get_unary_opcode(TokenType type);
OpCode get_postfix_opcode(TokenType type);

#endif

