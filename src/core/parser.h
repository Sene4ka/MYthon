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

ASTNode* parse_array_literal(Parser* parser);
ASTNode* parse_index(Parser* parser, ASTNode* array);

ASTNode* parse_if_statement(Parser* parser);
ASTNode* parse_block_statement(Parser* parser);
ASTNode* parse_function_statement(Parser* parser);
ASTNode* parse_return_statement(Parser* parser);
ASTNode* parse_break_statement(Parser* parser);

#endif
