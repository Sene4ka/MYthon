#ifndef MYTHON_PARSER_H
#define MYTHON_PARSER_H

#include "lexer.h"
#include "ast.h"

typedef struct {
    Lexer* lexer;
    Token current;
    Token previous;
    int error_count;
    int panic_mode;

    int in_loop;
    int in_function;

    const char* source_file;
} Parser;

void parser_init(Parser* parser, Lexer* lexer, const char* source_file);
ASTNode* parser_parse(Parser* parser);
int parser_has_errors(const Parser* parser);
int parser_error_count(const Parser* parser);

void parser_error(Parser* parser, const char* message);
void parser_error_at(Parser* parser, const Token* token, const char* message);
void parser_error_at_current(Parser* parser, const char* message);

void parser_advance(Parser* parser);
void parser_consume(Parser* parser, TokenType type, const char* message);
int parser_check(Parser* parser, TokenType type);
int parser_match(Parser* parser, TokenType type);

ASTNode* parse_expression(Parser* parser);
ASTNode* parse_statement(Parser* parser);
ASTNode* parse_declaration(Parser* parser);

ASTNode* parse_primary(Parser* parser);
ASTNode* parse_unary(Parser* parser);
ASTNode* parse_factor(Parser* parser);
ASTNode* parse_term(Parser* parser);
ASTNode* parse_comparison(Parser* parser);
ASTNode* parse_equality(Parser* parser);
ASTNode* parse_and(Parser* parser);
ASTNode* parse_or(Parser* parser);
ASTNode* parse_assignment(Parser* parser);

ASTNode* parse_if_statement(Parser* parser);
ASTNode* parse_for_statement(Parser* parser);
ASTNode* parse_block_statement(Parser* parser);
ASTNode* parse_return_statement(Parser* parser);
ASTNode* parse_function_statement(Parser* parser);

ASTNode* parse_argument_list(Parser* parser);
ASTNode* parse_parameter_list(Parser* parser, char*** params, size_t* param_count);
ASTNode* parse_array_literal(Parser* parser);
ASTNode* parse_call(Parser* parser, ASTNode* callee);
ASTNode* parse_index(Parser* parser, ASTNode* array);

#endif