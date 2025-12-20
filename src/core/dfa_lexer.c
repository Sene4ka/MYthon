#include "dfa_lexer.h"
#include "memory.h"
#include <ctype.h>
#include <string.h>
#include <stdio.h>

static DFAAction transition_table[256][64];
static int tables_built = 0;

static int char_class(unsigned char c) {
    if (c == 0) return 0;
    if (c == '(') return 1;
    if (c == ')') return 2;
    if (c == '{') return 3;
    if (c == '}') return 4;
    if (c == ',') return 5;
    if (c == '.') return 6;
    if (c == ';') return 7;
    if (c == ':') return 8;
    if (c == '?') return 9;
    if (c == '~') return 10;
    if (c == '!') return 11;
    if (c == '=') return 12;
    if (c == '<') return 13;
    if (c == '>') return 14;
    if (c == '&') return 15;
    if (c == '|') return 16;
    if (c == '^') return 17;
    if (c == '+') return 18;
    if (c == '-') return 19;
    if (c == '*') return 20;
    if (c == '/') return 21;
    if (c == '%') return 22;
    if (c == '"' || c == '\'') return 23;
    if (c >= '0' && c <= '9') return 24;
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') return 25;
    if (c == '\\') return 26;
    if (c == 'e' || c == 'E') return 27;
    if (c == '+' || c == '-') return 28;
    if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) return 29;
    if (c == 'u') return 30;
    if (c == 'x') return 31;
    if (c == '\n') return 32;
    if (c == ' ' || c == '\t' || c == '\r') return 33;
    return 34;
}

static void build_transition_table(void) {
    if (tables_built) return;

    for (int c = 0; c < 256; c++) {
        for (int s = 0; s < 64; s++) {
            transition_table[c][s].type = ACTION_ERROR;
            transition_table[c][s].next_state = STATE_ERROR;
        }
    }

    for (int c = 0; c < 256; c++) {
        int cls = char_class(c);

        transition_table[c][STATE_START].type = ACTION_CONTINUE;

        if (cls == 33) {
            transition_table[c][STATE_START].next_state = STATE_START;
        } else if (cls == 32) {
            transition_table[c][STATE_START].next_state = STATE_START;
        } else if (cls == 1) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_LEFT_PAREN;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 2) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_RIGHT_PAREN;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 3) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_LEFT_BRACE;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 4) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_RIGHT_BRACE;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 5) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_COMMA;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 6) {
            transition_table[c][STATE_START].next_state = STATE_DOT;
        } else if (cls == 7) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_SEMICOLON;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 8) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_COLON;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 9) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_QUESTION;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 10) {
            transition_table[c][STATE_START].type = ACTION_EMIT;
            transition_table[c][STATE_START].token_type = TOKEN_BIT_NOT;
            transition_table[c][STATE_START].next_state = STATE_ACCEPT;
        } else if (cls == 11) {
            transition_table[c][STATE_START].next_state = STATE_BANG;
        } else if (cls == 12) {
            transition_table[c][STATE_START].next_state = STATE_EQUAL;
        } else if (cls == 13) {
            transition_table[c][STATE_START].next_state = STATE_LESS;
        } else if (cls == 14) {
            transition_table[c][STATE_START].next_state = STATE_GREATER;
        } else if (cls == 15) {
            transition_table[c][STATE_START].next_state = STATE_AMP;
        } else if (cls == 16) {
            transition_table[c][STATE_START].next_state = STATE_PIPE;
        } else if (cls == 17) {
            transition_table[c][STATE_START].next_state = STATE_CARET;
        } else if (cls == 18) {
            transition_table[c][STATE_START].next_state = STATE_PLUS;
        } else if (cls == 19) {
            transition_table[c][STATE_START].next_state = STATE_MINUS;
        } else if (cls == 20) {
            transition_table[c][STATE_START].next_state = STATE_STAR;
        } else if (cls == 21) {
            transition_table[c][STATE_START].next_state = STATE_SLASH;
        } else if (cls == 22) {
            transition_table[c][STATE_START].next_state = STATE_PERCENT;
        } else if (cls == 23) {
            transition_table[c][STATE_START].next_state = STATE_STRING;
        } else if (cls == 24) {
            transition_table[c][STATE_START].next_state = STATE_NUMBER;
        } else if (cls == 25) {
            transition_table[c][STATE_START].next_state = STATE_IDENTIFIER;
        } else {
            transition_table[c][STATE_START].type = ACTION_ERROR;
            transition_table[c][STATE_START].next_state = STATE_ERROR;
        }
    }

    for (int c = 0; c < 256; c++) {
        int cls = char_class(c);

        if (cls == 24) {
            transition_table[c][STATE_NUMBER].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER].next_state = STATE_NUMBER;
        } else if (cls == 6) {
            transition_table[c][STATE_NUMBER].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER].next_state = STATE_NUMBER_DOT;
        } else if (cls == 27) {
            transition_table[c][STATE_NUMBER].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER].next_state = STATE_NUMBER_EXP;
        } else {
            transition_table[c][STATE_NUMBER].type = ACTION_RETURN;
            transition_table[c][STATE_NUMBER].token_type = TOKEN_NUMBER;
            transition_table[c][STATE_NUMBER].next_state = STATE_ACCEPT;
        }

        if (cls == 24) {
            transition_table[c][STATE_NUMBER_DOT].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER_DOT].next_state = STATE_NUMBER;
        } else if (cls == 27) {
            transition_table[c][STATE_NUMBER_DOT].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER_DOT].next_state = STATE_NUMBER_EXP;
        } else {
            transition_table[c][STATE_NUMBER_DOT].type = ACTION_RETURN;
            transition_table[c][STATE_NUMBER_DOT].token_type = TOKEN_NUMBER;
            transition_table[c][STATE_NUMBER_DOT].next_state = STATE_ACCEPT;
        }

        if (cls == 28) {
            transition_table[c][STATE_NUMBER_EXP].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER_EXP].next_state = STATE_NUMBER_EXP_SIGN;
        } else if (cls == 24) {
            transition_table[c][STATE_NUMBER_EXP].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER_EXP].next_state = STATE_NUMBER;
        } else {
            transition_table[c][STATE_NUMBER_EXP].type = ACTION_ERROR;
            transition_table[c][STATE_NUMBER_EXP].next_state = STATE_ERROR;
        }

        if (cls == 24) {
            transition_table[c][STATE_NUMBER_EXP_SIGN].type = ACTION_CONTINUE;
            transition_table[c][STATE_NUMBER_EXP_SIGN].next_state = STATE_NUMBER;
        } else {
            transition_table[c][STATE_NUMBER_EXP_SIGN].type = ACTION_ERROR;
            transition_table[c][STATE_NUMBER_EXP_SIGN].next_state = STATE_ERROR;
        }

        if (cls == 25 || cls == 24) {
            transition_table[c][STATE_IDENTIFIER].type = ACTION_CONTINUE;
            transition_table[c][STATE_IDENTIFIER].next_state = STATE_IDENTIFIER;
        } else {
            transition_table[c][STATE_IDENTIFIER].type = ACTION_RETURN;
            transition_table[c][STATE_IDENTIFIER].token_type = TOKEN_IDENTIFIER;
            transition_table[c][STATE_IDENTIFIER].next_state = STATE_ACCEPT;
        }

        if (c == '=') {
            transition_table[c][STATE_LESS].type = ACTION_EMIT;
            transition_table[c][STATE_LESS].token_type = TOKEN_LESS_EQUAL;
            transition_table[c][STATE_LESS].next_state = STATE_ACCEPT;
        } else if (c == '<') {
            transition_table[c][STATE_LESS].type = ACTION_CONTINUE;
            transition_table[c][STATE_LESS].next_state = STATE_LESS_LESS;
        } else {
            transition_table[c][STATE_LESS].type = ACTION_RETURN;
            transition_table[c][STATE_LESS].token_type = TOKEN_LESS;
            transition_table[c][STATE_LESS].next_state = STATE_ACCEPT;
        }

        if (c == '=') {
            transition_table[c][STATE_LESS_LESS].type = ACTION_EMIT;
            transition_table[c][STATE_LESS_LESS].token_type = TOKEN_BIT_LEFT_SHIFT_EQUAL;
            transition_table[c][STATE_LESS_LESS].next_state = STATE_ACCEPT;
        } else {
            transition_table[c][STATE_LESS_LESS].type = ACTION_RETURN;
            transition_table[c][STATE_LESS_LESS].token_type = TOKEN_BIT_LEFT_SHIFT;
            transition_table[c][STATE_LESS_LESS].next_state = STATE_ACCEPT;
        }

        if (c == '=') {
            transition_table[c][STATE_GREATER].type = ACTION_EMIT;
            transition_table[c][STATE_GREATER].token_type = TOKEN_GREATER_EQUAL;
            transition_table[c][STATE_GREATER].next_state = STATE_ACCEPT;
        } else if (c == '>') {
            transition_table[c][STATE_GREATER].type = ACTION_CONTINUE;
            transition_table[c][STATE_GREATER].next_state = STATE_GREATER_GREATER;
        } else {
            transition_table[c][STATE_GREATER].type = ACTION_RETURN;
            transition_table[c][STATE_GREATER].token_type = TOKEN_GREATER;
            transition_table[c][STATE_GREATER].next_state = STATE_ACCEPT;
        }

        if (c == '=') {
            transition_table[c][STATE_GREATER_GREATER].type = ACTION_EMIT;
            transition_table[c][STATE_GREATER_GREATER].token_type = TOKEN_BIT_RIGHT_SHIFT_EQUAL;
            transition_table[c][STATE_GREATER_GREATER].next_state = STATE_ACCEPT;
        } else {
            transition_table[c][STATE_GREATER_GREATER].type = ACTION_RETURN;
            transition_table[c][STATE_GREATER_GREATER].token_type = TOKEN_BIT_RIGHT_SHIFT;
            transition_table[c][STATE_GREATER_GREATER].next_state = STATE_ACCEPT;
        }
    }

    tables_built = 1;
}

void dfa_lexer_init(DFALexer* lexer, const unsigned char* source) {
    if (!tables_built) {
        dfa_lexer_build_tables();
    }

    lexer->state = STATE_START;
    lexer->start = source;
    lexer->current = source;
    lexer->line = 1;
    lexer->column = 1;
    lexer->error_count = 0;
    lexer->accepted_type = TOKEN_ERROR;
}

static Token make_dfa_token(DFALexer* lexer, TokenType type) {
    Token token;
    token.type = type;
    token.start = lexer->start;
    token.length = (size_t)(lexer->current - lexer->start);
    token.line = lexer->line;
    token.column = lexer->column - token.length;
    return token;
}

static Token error_dfa_token(DFALexer* lexer, const char* message) {
    lexer->error_count++;
    Token token;
    token.type = TOKEN_ERROR;
    token.start = (unsigned char*)message;
    token.length = strlen(message);
    token.line = lexer->line;
    token.column = lexer->column;
    return token;
}

Token dfa_lexer_next_token(DFALexer* lexer) {
    if (lexer->state == STATE_ACCEPT) {
        lexer->state = STATE_START;
        lexer->start = lexer->current;
    }

    if (*lexer->current == '\0') {
        return make_dfa_token(lexer, TOKEN_EOF);
    }

    while (1) {
        unsigned char c = *lexer->current;
        DFAAction action = transition_table[c][lexer->state];

        switch (action.type) {
            case ACTION_CONTINUE:
                lexer->state = action.next_state;
                lexer->current++;
                lexer->column++;
                if (c == '\n') {
                    lexer->line++;
                    lexer->column = 1;
                }
                break;

            case ACTION_EMIT:
                lexer->current++;
                lexer->column++;
                lexer->state = STATE_ACCEPT;
                return make_dfa_token(lexer, action.token_type);

            case ACTION_RETURN:
                lexer->state = STATE_ACCEPT;
                return make_dfa_token(lexer, action.token_type);

            case ACTION_ERROR:
                return error_dfa_token(lexer, "Unexpected character");
        }
    }
}

void dfa_lexer_build_tables(void) {
    build_transition_table();
}