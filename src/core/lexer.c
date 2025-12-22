#include "lexer.h"
#include "memory.h"
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

typedef struct {
    const char* keyword;
    TokenType type;
} KeywordEntry;

static const KeywordEntry keywords[] = {
    {"and", TOKEN_AND},
    {"class", TOKEN_CLASS},
    {"else", TOKEN_ELSE},
    {"false", TOKEN_FALSE},
    {"for", TOKEN_FOR},
    {"fn", TOKEN_FN},
    {"if", TOKEN_IF},
    {"nil", TOKEN_NIL},
    {"or", TOKEN_OR},
    {"return", TOKEN_RETURN},
    {"super", TOKEN_SUPER},
    {"this", TOKEN_THIS},
    {"true", TOKEN_TRUE},
    {"let", TOKEN_LET},
    {"var", TOKEN_VAR},
    {"while", TOKEN_WHILE},
    {"break", TOKEN_BREAK},
    {NULL, TOKEN_EOF}
};

static int is_at_end(const Lexer* lexer) {
    return *lexer->current == '\0';
}

static char advance(Lexer* lexer) {
    char c = *lexer->current;
    lexer->current++;
    lexer->column++;
    return c;
}

static char peek(const Lexer* lexer) {
    return *lexer->current;
}

static char peek_next(const Lexer* lexer) {
    if (is_at_end(lexer)) return '\0';
    return lexer->current[1];
}

static int match(Lexer* lexer, char expected) {
    if (is_at_end(lexer)) return 0;
    if (*lexer->current != expected) return 0;
    advance(lexer);
    return 1;
}

static Token make_token(const Lexer* lexer, TokenType type) {
    Token token;
    token.type = type;
    token.start = lexer->start;
    token.length = (size_t)(lexer->current - lexer->start);
    token.line = lexer->line;
    token.column = lexer->column - token.length;
    return token;
}

static Token error_token(Lexer* lexer, const char* message) {
    lexer->error_count++;
    Token token;
    token.type = TOKEN_ERROR;
    token.start = (const unsigned char*)message;
    token.length = strlen(message);
    token.line = lexer->line;
    token.column = lexer->column;
    return token;
}

static void skip_whitespace(Lexer* lexer) {
    while (1) {
        char c = peek(lexer);
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance(lexer);
                break;
            case '\n':
                advance(lexer);
                lexer->line++;
                lexer->column = 1;
                break;
            default:
                return;
        }
    }
}

void lexer_skip_comments(Lexer* lexer) {
    if (match(lexer, '/')) {
        if (match(lexer, '/')) {
            while (peek(lexer) != '\n' && !is_at_end(lexer)) {
                advance(lexer);
            }
        }
        else if (match(lexer, '*')) {
            int nesting = 1;
            while (nesting > 0 && !is_at_end(lexer)) {
                if (peek(lexer) == '\n') {
                    lexer->line++;
                    lexer->column = 1;
                }
                if (peek(lexer) == '/' && peek_next(lexer) == '*') {
                    advance(lexer);
                    advance(lexer);
                    nesting++;
                }
                else if (peek(lexer) == '*' && peek_next(lexer) == '/') {
                    advance(lexer);
                    advance(lexer);
                    nesting--;
                }
                else {
                    advance(lexer);
                }
            }
        }
        else {
            lexer->current--;
            lexer->column--;
        }
    }
}

static void handle_escape_sequence(Lexer* lexer) {
    if (is_at_end(lexer)) return;
    char c = peek(lexer);
    switch (c) {
        case 'n':
        case 't':
        case 'r':
        case '\\':
        case '"':
        case '\'':
        case '0':
        case 'a':
        case 'b':
        case 'f':
        case 'v':
            advance(lexer);
            break;
        case 'x':
            advance(lexer);
            if (isxdigit(peek(lexer))) {
                advance(lexer);
                if (isxdigit(peek(lexer))) {
                    advance(lexer);
                }
            }
            break;
        case 'u':
            advance(lexer);
            for (int i = 0; i < 4; i++) {
                if (!isxdigit(peek(lexer))) {
                    break;
                }
                advance(lexer);
            }
            break;
        default:
            break;
    }
}

static Token string(Lexer* lexer, char quote_type) {
    while (peek(lexer) != quote_type && !is_at_end(lexer)) {
        if (peek(lexer) == '\n') {
            lexer->line++;
            lexer->column = 1;
        }
        if (peek(lexer) == '\\') {
            advance(lexer);
            handle_escape_sequence(lexer);
        }
        else {
            advance(lexer);
        }
    }
    if (is_at_end(lexer)) {
        return error_token(lexer, "Unterminated string");
    }
    advance(lexer);
    return make_token(lexer, TOKEN_STRING);
}

static Token number(Lexer* lexer) {
    while (isdigit(peek(lexer))) {
        advance(lexer);
    }
    if (peek(lexer) == '.' && isdigit(peek_next(lexer))) {
        advance(lexer);
        while (isdigit(peek(lexer))) {
            advance(lexer);
        }
    }
    if (peek(lexer) == 'e' || peek(lexer) == 'E') {
        char next = peek_next(lexer);
        if (next == '+' || next == '-' || isdigit(next)) {
            advance(lexer);
            if (peek(lexer) == '+' || peek(lexer) == '-') {
                advance(lexer);
            }
            while (isdigit(peek(lexer))) {
                advance(lexer);
            }
        }
    }
    return make_token(lexer, TOKEN_NUMBER);
}

static int is_alpha(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           c == '_';
}

static int is_alphanumeric(char c) {
    return is_alpha(c) || isdigit(c);
}

static Token identifier(Lexer* lexer) {
    while (is_alphanumeric(peek(lexer))) {
        advance(lexer);
    }
    size_t length = (size_t)(lexer->current - lexer->start);
    for (const KeywordEntry* entry = keywords; entry->keyword != NULL; entry++) {
        if (strlen(entry->keyword) == length &&
            memcmp(lexer->start, entry->keyword, length) == 0) {
            return make_token(lexer, entry->type);
        }
    }
    return make_token(lexer, TOKEN_IDENTIFIER);
}

static Token scan_token(Lexer* lexer) {
    skip_whitespace(lexer);
    lexer_skip_comments(lexer);
    skip_whitespace(lexer);

    lexer->start = lexer->current;

    if (is_at_end(lexer)) {
        return make_token(lexer, TOKEN_EOF);
    }

    char c = advance(lexer);

    if (isdigit(c)) {
        return number(lexer);
    }

    if (is_alpha(c)) {
        return identifier(lexer);
    }

    switch (c) {
        case '(': return make_token(lexer, TOKEN_LEFT_PAREN);
        case ')': return make_token(lexer, TOKEN_RIGHT_PAREN);
        case '{': return make_token(lexer, TOKEN_LEFT_BRACE);
        case '}': return make_token(lexer, TOKEN_RIGHT_BRACE);
        case '[': return make_token(lexer, TOKEN_LEFT_BRACKET);
        case ']': return make_token(lexer, TOKEN_RIGHT_BRACKET);
        case ',': return make_token(lexer, TOKEN_COMMA);
        case '.': return make_token(lexer, TOKEN_DOT);
        case ';': return make_token(lexer, TOKEN_SEMICOLON);
        case '*':
            if (match(lexer, '=')) return make_token(lexer, TOKEN_STAR_EQUAL);
            return make_token(lexer, TOKEN_STAR);
        case '%':
            if (match(lexer, '=')) return make_token(lexer, TOKEN_PERCENT_EQUAL);
            return make_token(lexer, TOKEN_PERCENT);
        case '~': return make_token(lexer, TOKEN_BIT_NOT);
        case '?': return make_token(lexer, TOKEN_QUESTION);
        case ':': return make_token(lexer, TOKEN_COLON);
        case '-':
            if (match(lexer, '-')) return make_token(lexer, TOKEN_MINUS_MINUS);
            if (match(lexer, '=')) return make_token(lexer, TOKEN_MINUS_EQUAL);
            return make_token(lexer, TOKEN_MINUS);
        case '+':
            if (match(lexer, '+')) return make_token(lexer, TOKEN_PLUS_PLUS);
            if (match(lexer, '=')) return make_token(lexer, TOKEN_PLUS_EQUAL);
            return make_token(lexer, TOKEN_PLUS);
        case '!':
            if (match(lexer, '=')) return make_token(lexer, TOKEN_BANG_EQUAL);
            return make_token(lexer, TOKEN_BANG);
        case '=':
            if (match(lexer, '=')) return make_token(lexer, TOKEN_EQUAL_EQUAL);
            return make_token(lexer, TOKEN_EQUAL);
        case '<':
            if (match(lexer, '<')) {
                if (match(lexer, '=')) return make_token(lexer, TOKEN_BIT_LEFT_SHIFT_EQUAL);
                return make_token(lexer, TOKEN_BIT_LEFT_SHIFT);
            }
            if (match(lexer, '=')) return make_token(lexer, TOKEN_LESS_EQUAL);
            return make_token(lexer, TOKEN_LESS);
        case '>':
            if (match(lexer, '>')) {
                if (match(lexer, '=')) return make_token(lexer, TOKEN_BIT_RIGHT_SHIFT_EQUAL);
                return make_token(lexer, TOKEN_BIT_RIGHT_SHIFT);
            }
            if (match(lexer, '=')) return make_token(lexer, TOKEN_GREATER_EQUAL);
            return make_token(lexer, TOKEN_GREATER);
        case '/':
            if (match(lexer, '=')) return make_token(lexer, TOKEN_SLASH_EQUAL);
            return make_token(lexer, TOKEN_SLASH);
        case '&':
            if (match(lexer, '&')) return make_token(lexer, TOKEN_AND);
            if (match(lexer, '=')) return make_token(lexer, TOKEN_BIT_AND_EQUAL);
            return make_token(lexer, TOKEN_BIT_AND);
        case '|':
            if (match(lexer, '|')) return make_token(lexer, TOKEN_OR);
            if (match(lexer, '=')) return make_token(lexer, TOKEN_BIT_OR_EQUAL);
            return make_token(lexer, TOKEN_BIT_OR);
        case '^':
            if (match(lexer, '=')) return make_token(lexer, TOKEN_BIT_XOR_EQUAL);
            return make_token(lexer, TOKEN_BIT_XOR);
        case '"':
        case '\'':
            return string(lexer, c);
        default:
            return error_token(lexer, "Unexpected character");
    }
}

void lexer_init(Lexer* lexer, const unsigned char* source) {
    lexer->source = source;
    lexer->start = source;
    lexer->current = source;
    lexer->line = 1;
    lexer->column = 1;
    lexer->error_count = 0;
}

Token lexer_next_token(Lexer* lexer) {
    return scan_token(lexer);
}

int lexer_has_errors(const Lexer* lexer) {
    return lexer->error_count > 0;
}

const char* token_type_to_string(TokenType type) {
    switch (type) {
        case TOKEN_LEFT_PAREN: return "LEFT_PAREN";
        case TOKEN_RIGHT_PAREN: return "RIGHT_PAREN";
        case TOKEN_LEFT_BRACE: return "LEFT_BRACE";
        case TOKEN_RIGHT_BRACE: return "RIGHT_BRACE";
        case TOKEN_LEFT_BRACKET: return "LEFT_BRACKET";
        case TOKEN_RIGHT_BRACKET: return "RIGHT_BRACKET";
        case TOKEN_COMMA: return "COMMA";
        case TOKEN_DOT: return "DOT";
        case TOKEN_MINUS: return "MINUS";
        case TOKEN_MINUS_MINUS: return "MINUS_MINUS";
        case TOKEN_MINUS_EQUAL: return "MINUS_EQUAL";
        case TOKEN_PLUS: return "PLUS";
        case TOKEN_PLUS_PLUS: return "PLUS_PLUS";
        case TOKEN_PLUS_EQUAL: return "PLUS_EQUAL";
        case TOKEN_SEMICOLON: return "SEMICOLON";
        case TOKEN_SLASH: return "SLASH";
        case TOKEN_SLASH_EQUAL: return "SLASH_EQUAL";
        case TOKEN_STAR: return "STAR";
        case TOKEN_STAR_EQUAL: return "STAR_EQUAL";
        case TOKEN_PERCENT: return "PERCENT";
        case TOKEN_PERCENT_EQUAL: return "PERCENT_EQUAL";
        case TOKEN_BANG: return "BANG";
        case TOKEN_BANG_EQUAL: return "BANG_EQUAL";
        case TOKEN_EQUAL: return "EQUAL";
        case TOKEN_EQUAL_EQUAL: return "EQUAL_EQUAL";
        case TOKEN_GREATER: return "GREATER";
        case TOKEN_GREATER_EQUAL: return "GREATER_EQUAL";
        case TOKEN_LESS: return "LESS";
        case TOKEN_LESS_EQUAL: return "LESS_EQUAL";
        case TOKEN_COLON: return "COLON";
        case TOKEN_QUESTION: return "QUESTION";
        case TOKEN_BIT_AND: return "BIT_AND";
        case TOKEN_BIT_OR: return "BIT_OR";
        case TOKEN_BIT_XOR: return "BIT_XOR";
        case TOKEN_BIT_NOT: return "BIT_NOT";
        case TOKEN_BIT_LEFT_SHIFT: return "BIT_LEFT_SHIFT";
        case TOKEN_BIT_RIGHT_SHIFT: return "BIT_RIGHT_SHIFT";
        case TOKEN_BIT_AND_EQUAL: return "BIT_AND_EQUAL";
        case TOKEN_BIT_OR_EQUAL: return "BIT_OR_EQUAL";
        case TOKEN_BIT_XOR_EQUAL: return "BIT_XOR_EQUAL";
        case TOKEN_BIT_LEFT_SHIFT_EQUAL: return "BIT_LEFT_SHIFT_EQUAL";
        case TOKEN_BIT_RIGHT_SHIFT_EQUAL: return "BIT_RIGHT_SHIFT_EQUAL";
        case TOKEN_IDENTIFIER: return "IDENTIFIER";
        case TOKEN_STRING: return "STRING";
        case TOKEN_NUMBER: return "NUMBER";
        case TOKEN_AND: return "AND";
        case TOKEN_CLASS: return "CLASS";
        case TOKEN_ELSE: return "ELSE";
        case TOKEN_FALSE: return "FALSE";
        case TOKEN_FOR: return "FOR";
        case TOKEN_FN: return "FN";
        case TOKEN_IF: return "IF";
        case TOKEN_NIL: return "NIL";
        case TOKEN_OR: return "OR";
        case TOKEN_RETURN: return "RETURN";
        case TOKEN_SUPER: return "SUPER";
        case TOKEN_THIS: return "THIS";
        case TOKEN_TRUE: return "TRUE";
        case TOKEN_LET: return "LET";
        case TOKEN_WHILE: return "WHILE";
        case TOKEN_BREAK: return "BREAK";
        case TOKEN_EOF: return "EOF";
        case TOKEN_ERROR: return "ERROR";
        default: return "UNKNOWN";
    }
}

char* token_copy_text(const Token* token) {
    if (token->type == TOKEN_EOF) {
        char* result = ALLOCATE(char, 1);
        result[0] = '\0';
        return result;
    }
    if (token->type == TOKEN_ERROR) {
        return copy_string((const char*)token->start);
    }
    char* result = ALLOCATE(char, token->length + 1);
    memcpy(result, token->start, token->length);
    result[token->length] = '\0';
    return result;
}

double token_as_number(const Token* token) {
    if (token->type != TOKEN_NUMBER) {
        return 0.0;
    }
    char* text = token_copy_text(token);
    double value = atof(text);
    FREE_ARRAY(char, text, token->length + 1);
    return value;
}

int token_is_keyword(const Token* token) {
    return token->type >= TOKEN_AND && token->type <= TOKEN_WHILE;
}
