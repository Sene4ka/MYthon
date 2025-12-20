#include "dfa_lexer.h"
#include "memory.h"
#include <string.h>

static const DFAAction fast_table[256][64];

static Token fast_lexer_next_token_internal(DFALexer* lexer) {
    if (lexer->state == STATE_ACCEPT) {
        lexer->state = STATE_START;
        lexer->start = lexer->current;
    }

    if (*lexer->current == '\0') {
        Token token;
        token.type = TOKEN_EOF;
        token.start = lexer->current;
        token.length = 0;
        token.line = lexer->line;
        token.column = lexer->column;
        return token;
    }

    while (1) {
        unsigned char c = *lexer->current;
        const DFAAction* action = &fast_table[c][lexer->state];

        switch (action->type) {
            case ACTION_CONTINUE:
                lexer->state = action->next_state;
                lexer->current++;
                lexer->column++;
                if (c == '\n') {
                    lexer->line++;
                    lexer->column = 1;
                }
                continue;

            case ACTION_EMIT:
                lexer->current++;
                lexer->column++;
                lexer->state = STATE_ACCEPT;
                break;

            case ACTION_RETURN:
                lexer->state = STATE_ACCEPT;
                break;

            case ACTION_ERROR:
                lexer->error_count++;
                Token token;
                token.type = TOKEN_ERROR;
                token.start = (unsigned char*)"Unexpected character";
                token.length = 19;
                token.line = lexer->line;
                token.column = lexer->column;
                return token;
        }

        Token token;
        token.type = action->token_type;
        token.start = lexer->start;
        token.length = (size_t)(lexer->current - lexer->start);
        token.line = lexer->line;
        token.column = lexer->column - token.length;
        return token;
    }
}

Token fast_lexer_next_token(DFALexer* lexer) {
    Token token = fast_lexer_next_token_internal(lexer);

    if (token.type == TOKEN_IDENTIFIER) {
        const unsigned char* start = token.start;
        size_t length = token.length;

        static const struct {
            const char* keyword;
            TokenType type;
        } keywords[] = {
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
                {"var", TOKEN_VAR},
                {"while", TOKEN_WHILE},
                {NULL, TOKEN_ERROR}
        };

        for (int i = 0; keywords[i].keyword != NULL; i++) {
            if (strlen(keywords[i].keyword) == length &&
                memcmp(start, keywords[i].keyword, length) == 0) {
                token.type = keywords[i].type;
                break;
            }
        }
    }

    return token;
}