#ifndef MYTHON_DFA_LEXER_H
#define MYTHON_DFA_LEXER_H

#include "lexer.h"

typedef enum {
    STATE_START,
    STATE_NUMBER,
    STATE_NUMBER_DOT,
    STATE_NUMBER_EXP,
    STATE_NUMBER_EXP_SIGN,
    STATE_IDENTIFIER,
    STATE_STRING,
    STATE_STRING_ESC,
    STATE_STRING_HEX1,
    STATE_STRING_HEX2,
    STATE_STRING_UNI1,
    STATE_STRING_UNI2,
    STATE_STRING_UNI3,
    STATE_STRING_UNI4,
    STATE_SLASH,
    STATE_SLASH_SLASH,
    STATE_SLASH_STAR,
    STATE_SLASH_STAR_STAR,
    STATE_LESS,
    STATE_LESS_LESS,
    STATE_GREATER,
    STATE_GREATER_GREATER,
    STATE_BANG,
    STATE_EQUAL,
    STATE_AMP,
    STATE_PIPE,
    STATE_CARET,
    STATE_PLUS,
    STATE_MINUS,
    STATE_STAR,
    STATE_PERCENT,
    STATE_DOT,
    STATE_ERROR,
    STATE_ACCEPT
} DFAState;

typedef enum {
    ACTION_CONTINUE,
    ACTION_EMIT,
    ACTION_ERROR,
    ACTION_RETURN
} ActionType;

typedef struct {
    ActionType type;
    TokenType token_type;
    DFAState next_state;
} DFAAction;

typedef struct {
    DFAState state;
    const unsigned char* start;
    const unsigned char* current;
    int line;
    int column;
    int error_count;
    TokenType accepted_type;
} DFALexer;

void dfa_lexer_init(DFALexer* lexer, const unsigned char* source);
Token dfa_lexer_next_token(DFALexer* lexer);
void dfa_lexer_build_tables(void);

#endif