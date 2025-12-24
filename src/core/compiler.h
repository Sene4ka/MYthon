#ifndef MYTHON_COMPILER_H
#define MYTHON_COMPILER_H

#include "ast.h"
#include "bytecode.h"
#include "lexer.h"

/* Результат компиляции */
typedef enum {
    COMPILE_SUCCESS,
    COMPILE_ERROR,
    COMPILE_SYNTAX_ERROR
} CompileResult;

/* Локальные переменные в текущей функции */
typedef struct {
    char** names;
    int* depths;
    int* is_captured;   /* 1 если локал захвачен как upvalue */
    int count;
    int capacity;
} LocalTable;

/* Запись об одном upvalue функции: либо локал текущей, либо upvalue внешней */
typedef struct {
    int is_local;   /* 1 если захватываем локал текущей функции */
    int index;      /* индекс локала или внешнего upvalue */
} UpvalueEntry;

/* Состояние одной функции во время компиляции */
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

/* Состояние для break/continue внутри циклов */
typedef struct {
    int loop_depth;
    size_t* break_jumps;
    int break_count;
    int break_capacity;
    size_t* continue_jumps;
    int continue_count;
    int continue_capacity;
} LoopState;

/* Текущее состояние компиляции всего модуля */
typedef struct {
    FunctionState* functions;
    int function_depth;     /* глубина вложенности функций (0 = top‑level) */
    int function_capacity;
    int current_function;   /* индекс в массиве functions */

    LoopState loop;

    int error_count;
    const char* error_message;
    const char* source_file;
    int current_line;

    int locals_at_toplevel; /* сколько локалов было в корневой функции на входе */
    int native_global_offset;

    int debug;
} Compiler;

/* ===== lifecycle ===== */

void compiler_init(Compiler* compiler, const char* source_file);
void compiler_free(Compiler* compiler);

/* Компиляция целого AST в Bytecode (модуль) */
CompileResult compiler_compile(Compiler* compiler, const ASTNode* ast, Bytecode* bytecode);

/* ===== node-based compile API ===== */

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

/* ===== locals / upvalues ===== */

/* поиск локала в текущей функции, -1 если не найден */
int compiler_resolve_local(Compiler* compiler, const char* name, int length);

/* добавить локал (в текущую функцию), вернуть его индекс или -1 при ошибке */
int compiler_add_local(Compiler* compiler, const char* name, int length);

/* найти/создать upvalue для текущей функции, вернуть индекс в таблице upvalues или -1 */
int compiler_resolve_upvalue(Compiler* compiler, const char* name, int length);

int compiler_resolve_global(Compiler* compiler, Bytecode* bytecode, const char* name);
int compiler_define_global(Compiler* compiler, Bytecode* bytecode, const char* name, int const_idx);

/* управление scope внутри текущей функции */
void compiler_begin_scope(Compiler* compiler);
void compiler_end_scope(Compiler* compiler, Bytecode* bytecode, int start_local_count);

/* ===== loops ===== */
void compiler_begin_loop(Compiler* compiler);
void compiler_end_loop(Compiler* compiler, Bytecode* bytecode);

/* допатчить все break/continue в текущем loop до адреса target_ip */
void compiler_patch_breaks(Compiler* compiler, Bytecode* bytecode, size_t target_ip);
void compiler_patch_continues(Compiler* compiler, Bytecode* bytecode, size_t target_ip);

/* ===== functions (включая main) ===== */

/* начать новую функцию (включая main как функцию 0) */
void compiler_begin_function(Compiler* compiler,
                             Bytecode* bytecode,
                             const char* name,
                             int arity,
                             int line,
                             int* out_func_index);

/* завершить текущую функцию: выставить code_end, max_locals, upvalues[] в Bytecode */
void compiler_end_function(Compiler* compiler, Bytecode* bytecode);

/* ===== emitting bytecode ===== */

void emit_byte(Compiler* compiler, Bytecode* bytecode, uint8_t byte);
void emit_op(Compiler* compiler, Bytecode* bytecode, OpCode op);
void emit_u8(Compiler* compiler, Bytecode* bytecode, uint8_t value);
void emit_u16(Compiler* compiler, Bytecode* bytecode, uint16_t value);

/* константы в Bytecode */
int compiler_add_const_int(Bytecode* bytecode, int64_t value);
int compiler_add_const_float(Bytecode* bytecode, double value);
int compiler_add_const_string(Bytecode* bytecode, const char* value);

/* прыжки (16‑битное смещение от адреса после операнда) */
void emit_jump_u16(Compiler* compiler, Bytecode* bytecode,
                   OpCode op, size_t* patch_pos);
/* patch_pos указывает на старший байт смещения */
void patch_jump_u16(Bytecode* bytecode, size_t patch_pos);

/* цикл (jump назад) */
void emit_loop_u16(Compiler* compiler, Bytecode* bytecode, size_t loop_start);

/* ===== ошибки / utils ===== */

void compiler_error(Compiler* compiler, const char* message);
void compiler_error_at_line(Compiler* compiler, int line, const char* message);

/* маппинг операторов в opcodes нового байткода */
int  get_operator_precedence(TokenType type);
OpCode get_binary_opcode(TokenType type);
OpCode get_unary_opcode(TokenType type);
OpCode get_postfix_opcode(TokenType type);

#endif /* MYTHON_COMPILER_H */

