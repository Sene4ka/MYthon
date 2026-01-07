#ifndef MYTHON_LEXER_H
#define MYTHON_LEXER_H

#include <stddef.h>

typedef enum {
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_LEFT_BRACKET,
    TOKEN_RIGHT_BRACKET,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_MINUS_MINUS,
    TOKEN_PLUS,
    TOKEN_PLUS_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    TOKEN_PERCENT,

    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    TOKEN_COLON,
    TOKEN_QUESTION,

    TOKEN_BIT_AND,
    TOKEN_BIT_OR,
    TOKEN_BIT_XOR,
    TOKEN_BIT_NOT,
    TOKEN_BIT_LEFT_SHIFT,
    TOKEN_BIT_RIGHT_SHIFT,

    TOKEN_BIT_AND_EQUAL,
    TOKEN_BIT_OR_EQUAL,
    TOKEN_BIT_XOR_EQUAL,
    TOKEN_BIT_LEFT_SHIFT_EQUAL,
    TOKEN_BIT_RIGHT_SHIFT_EQUAL,

    TOKEN_PLUS_EQUAL,
    TOKEN_MINUS_EQUAL,
    TOKEN_STAR_EQUAL,
    TOKEN_SLASH_EQUAL,
    TOKEN_PERCENT_EQUAL,

    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    TOKEN_AND,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_LET,
    TOKEN_WHILE,
    TOKEN_BREAK,

    TOKEN_EOF,
    TOKEN_ERROR
} TokenType;

typedef struct {
    TokenType type;
    const unsigned char* start;
    size_t length;
    int line;
    int column;
} Token;

typedef struct {
    const unsigned char* source;
    const unsigned char* start;
    const unsigned char* current;
    int line;
    int column;
    int error_count;
} Lexer;

void lexer_init(Lexer* lexer, const unsigned char* source);
Token lexer_next_token(Lexer* lexer);
int lexer_has_errors(const Lexer* lexer);
const char* token_type_to_string(TokenType type);
char* token_copy_text(const Token* token);
double token_as_number(const Token* token);
int token_is_keyword(const Token* token);
void lexer_skip_comments(Lexer* lexer);

#endif
