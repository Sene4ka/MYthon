#ifndef MYTHON_AST_H
#define MYTHON_AST_H

#include "lexer.h"

typedef enum {
    NODE_BINARY_EXPR,
    NODE_TERNARY_EXPR,
    NODE_UNARY_EXPR,
    NODE_POSTFIX_EXPR,
    NODE_LITERAL_EXPR,
    NODE_VARIABLE_EXPR,
    NODE_ASSIGN_EXPR,
    NODE_CALL_EXPR,
    NODE_GROUP_EXPR,
    NODE_ARRAY_EXPR,
    NODE_INDEX_EXPR,
    NODE_LOGICAL_EXPR,
    NODE_MEMBER_EXPR,

    NODE_EXPR_STMT,
    NODE_BLOCK_STMT,
    NODE_IF_STMT,
    NODE_FOR_STMT,
    NODE_RETURN_STMT,
    NODE_FUNCTION_STMT,
    NODE_BREAK_STMT,
    NODE_CONTINUE_STMT,

    NODE_PROGRAM
} NodeType;

typedef struct ASTNode ASTNode;

struct ASTNode {
    NodeType type;
    int line;
    int column;

    ASTNode* next;

    union {
        struct {
            ASTNode* left;
            ASTNode* right;
            TokenType operator;
        } binary;

        struct {
            ASTNode* condition;
            ASTNode* then_expr;
            ASTNode* else_expr;
        } ternary;

        struct {
            ASTNode* operand;
            TokenType operator;
        } unary;

        struct {
            ASTNode* operand;
            TokenType operator;
        } postfix;

        struct {
            enum {
                LIT_NUMBER,
                LIT_STRING,
                LIT_BOOL,
                LIT_NIL
            } value_type;
            union {
                double number;
                char* string;
                int boolean;
            } value;
        } literal;

        struct {
            char* name;
            size_t name_length;
        } variable;

        struct {
            ASTNode* target;
            ASTNode* value;
        } assign;

        struct {
            ASTNode* callee;
            ASTNode** arguments;
            size_t arg_count;
        } call;

        struct {
            ASTNode** elements;
            size_t element_count;
        } array;

        struct {
            ASTNode* array;
            ASTNode* index;
        } index;

        struct {
            ASTNode* left;
            ASTNode* right;
            TokenType operator;
        } logical;

        struct {
            ASTNode* object;
            char* name;
            size_t name_length;
        } member;

        struct {
            ASTNode* statements;
        } block;

        struct {
            ASTNode* condition;
            ASTNode* then_branch;
            ASTNode* else_branch;
        } if_stmt;

        struct {
            ASTNode* initializer;
            ASTNode* condition;
            ASTNode* increment;
            ASTNode* body;
        } for_stmt;

        struct {
            char* name;
            size_t name_length;
            char** parameters;
            size_t param_count;
            ASTNode* body;
        } function_stmt;

        struct {
            ASTNode* value;
        } return_stmt;

        struct {
            ASTNode* statements;
        } program;

        struct {
            ASTNode* expression;
        } expr_stmt;

        struct {
            ASTNode* target;
            ASTNode* value;
        } continue_stmt;
    };
};

ASTNode* ast_new_node(NodeType type, int line, int column);
ASTNode* ast_new_binary(ASTNode* left, ASTNode* right, TokenType operator, int line, int column);
ASTNode* ast_new_ternary(ASTNode* condition, ASTNode* then_expr, ASTNode* else_expr, int line, int column);
ASTNode* ast_new_unary(ASTNode* operand, TokenType operator, int line, int column);
ASTNode* ast_new_postfix(ASTNode* operand, TokenType operator, int line, int column);
ASTNode* ast_new_number_literal(double value, int line, int column);
ASTNode* ast_new_string_literal(const char* value, int line, int column);
ASTNode* ast_new_bool_literal(int value, int line, int column);
ASTNode* ast_new_nil_literal(int line, int column);
ASTNode* ast_new_variable(const char* name, size_t length, int line, int column);
ASTNode* ast_new_assign(ASTNode* target, ASTNode* value, int line, int column);
ASTNode* ast_new_call(ASTNode* callee, ASTNode** args, size_t arg_count, int line, int column);
ASTNode* ast_new_block(ASTNode* statements, int line, int column);
ASTNode* ast_new_if(ASTNode* condition, ASTNode* then_branch, ASTNode* else_branch, int line, int column);
ASTNode* ast_new_for(ASTNode* init, ASTNode* condition, ASTNode* increment, ASTNode* body, int line, int column);
ASTNode* ast_new_function(const char* name, size_t length, char** params, size_t param_count, ASTNode* body, int line, int column);
ASTNode* ast_new_return(ASTNode* value, int line, int column);
ASTNode* ast_new_program(ASTNode* statements, int line, int column);
ASTNode* ast_new_expr_stmt(ASTNode* expression, int line, int column);
ASTNode* ast_new_array(ASTNode** elements, size_t count, int line, int column);
ASTNode* ast_new_index(ASTNode* array, ASTNode* index, int line, int column);
ASTNode* ast_new_logical(ASTNode* left, ASTNode* right, TokenType operator, int line, int column);
ASTNode* ast_new_break(int line, int column);
ASTNode* ast_new_continue(ASTNode* target, ASTNode* value, int line, int column);
ASTNode* ast_new_group(ASTNode* expression, int line, int column);
ASTNode* ast_new_member(ASTNode* object, const char* name, size_t length, int line, int column);

void ast_free(ASTNode* node);
void ast_free_array(ASTNode** nodes, size_t count);

void ast_print(const ASTNode* node, int indent);
const char* node_type_to_string(NodeType type);

int ast_node_is_statement(NodeType type);
int ast_node_is_expression(NodeType type);
ASTNode* ast_last_statement(ASTNode* statements);

int get_operator_precedence(TokenType type);
int is_assignment_operator(TokenType type);
int is_comparison_operator(TokenType type);
int is_arithmetic_operator(TokenType type);
int is_logical_operator(TokenType type);
int is_bitwise_operator(TokenType type);
int is_unary_operator(TokenType type);
int is_postfix_operator(TokenType type);

#endif
