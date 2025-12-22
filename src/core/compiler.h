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
    struct {
        char** names;
        int* depths;
        int count;
        int capacity;
    } locals;

    int scope_depth;

    struct {
        char* current_function;
        int function_depth;
        int return_count;
    } function;

    struct {
        int loop_depth;
        size_t* break_jumps;
        int break_count;
        int break_capacity;
        size_t* continue_jumps;
        int continue_count;
        int continue_capacity;
    } loop;

    int error_count;
    const char* error_message;

    const char* source_file;
    int current_line;
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

CompileResult compile_if(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_for(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_function(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_return(Compiler* compiler, const ASTNode* node, Bytecode* bytecode);
CompileResult compile_break(Compiler* compiler, Bytecode* bytecode);
CompileResult compile_continue(Compiler* compiler, Bytecode* bytecode);

int compiler_resolve_local(Compiler* compiler, const char* name, int length);
int compiler_add_local(Compiler* compiler, const char* name, int length);
void compiler_begin_scope(Compiler* compiler);
void compiler_end_scope(Compiler* compiler, Bytecode* bytecode, int start_count);

void compiler_begin_loop(Compiler* compiler);
void compiler_end_loop(Compiler* compiler, Bytecode* bytecode);
void compiler_patch_breaks(Compiler* compiler, Bytecode* bytecode, size_t target);
void compiler_patch_continues(Compiler* compiler, Bytecode* bytecode, size_t target);

void compiler_begin_function(Compiler* compiler, const char* name);
void compiler_end_function(Compiler* compiler);

void emit_byte(Compiler* compiler, Bytecode* bytecode, uint8_t byte);
void emit_op(Compiler* compiler, Bytecode* bytecode, OpCode op);
void emit_i8(Compiler* compiler, Bytecode* bytecode, int8_t value);
void emit_i16(Compiler* compiler, Bytecode* bytecode, int16_t value);
void emit_i32(Compiler* compiler, Bytecode* bytecode, int32_t value);
void emit_u8(Compiler* compiler, Bytecode* bytecode, uint8_t value);
void emit_u16(Compiler* compiler, Bytecode* bytecode, uint16_t value);
void emit_u32(Compiler* compiler, Bytecode* bytecode, uint32_t value);
void emit_const(Compiler* compiler, Bytecode* bytecode, int index);
void emit_jump(Compiler* compiler, Bytecode* bytecode, OpCode op, size_t* jump_address);
void emit_loop(Compiler* compiler, Bytecode* bytecode, size_t loop_start);
void patch_jump(Compiler* compiler, Bytecode* bytecode, size_t jump_address);

void compiler_error(Compiler* compiler, const char* message);
void compiler_error_at_line(Compiler* compiler, int line, const char* message);

void optimize_bytecode(Bytecode* bytecode);

int get_operator_precedence(TokenType type);
OpCode get_binary_opcode(TokenType type);
OpCode get_unary_opcode(TokenType type);
OpCode get_postfix_opcode(TokenType type);
int should_emit_short(int value);

#endif
