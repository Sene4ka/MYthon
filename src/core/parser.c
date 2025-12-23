#include "parser.h"
#include "memory.h"
#include <stdio.h>
#include <string.h>

static void synchronize(Parser* parser);
static ASTNode* parse_declaration(Parser* parser);
static ASTNode* parse_var_declaration(Parser* parser);
static ASTNode* parse_while_statement(Parser* parser);
static ASTNode* parse_expression_statement(Parser* parser);
static ASTNode* parse_anonymous_function(Parser* parser);
static ASTNode** parse_argument_list(Parser* parser, size_t* arg_count);
static ASTNode* parse_for_statement(Parser* parser);
static ASTNode* parse_block_statement_from_brace(Parser* parser);
static ASTNode* parse_call(Parser* parser, ASTNode* callee);
static int parse_parameter_list(Parser* parser, char*** params, size_t* param_count);
static void consume_semicolon(Parser* parser);
static ASTNode* parse_var_declaration_no_semi(Parser* parser);

ASTNode* parse_assignment(Parser* parser);
ASTNode* parse_or(Parser* parser);
ASTNode* parse_and(Parser* parser);
ASTNode* parse_equality(Parser* parser);
ASTNode* parse_comparison(Parser* parser);
ASTNode* parse_bitwise_or(Parser* parser);
ASTNode* parse_bitwise_xor(Parser* parser);
ASTNode* parse_bitwise_and(Parser* parser);
ASTNode* parse_shift(Parser* parser);
ASTNode* parse_term(Parser* parser);
ASTNode* parse_factor(Parser* parser);
ASTNode* parse_unary(Parser* parser);
ASTNode* parse_postfix(Parser* parser);
ASTNode* parse_primary(Parser* parser);

void parser_init(Parser* parser, Lexer* lexer, const char* source_file) {
    parser->lexer = lexer;
    parser->current = lexer_next_token(lexer);
    parser->previous.type = TOKEN_EOF;
    parser->previous.start = NULL;
    parser->previous.length = 0;
    parser->previous.line = 1;
    parser->previous.column = 1;
    parser->error_count = 0;
    parser->panic_mode = 0;
    parser->in_loop = 0;
    parser->in_function = 0;
    parser->source_file = source_file;
}

void synchronize(Parser* parser) {
    parser->panic_mode = 0;
    while (parser->current.type != TOKEN_EOF) {
        if (parser->previous.type == TOKEN_SEMICOLON) return;
        switch (parser->current.type) {
            case TOKEN_CLASS:
            case TOKEN_FN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
                return;
            default:
                break;
        }
        parser_advance(parser);
    }
}

void parser_error(Parser* parser, const char* message) {
    if (parser->panic_mode) return;
    parser->panic_mode = 1;
    parser->error_count++;
    fprintf(stderr, "[%s:%d:%d] Ошибка",
            parser->source_file ? parser->source_file : "<source>",
            parser->previous.line,
            parser->previous.column);
    if (parser->previous.type == TOKEN_EOF) {
        fprintf(stderr, " в конце файла");
    } else if (parser->previous.type != TOKEN_ERROR) {
        char* text = token_copy_text(&parser->previous);
        fprintf(stderr, " на токене '%.*s'", (int)parser->previous.length, text);
        FREE_ARRAY(char, text, parser->previous.length + 1);
    }
    fprintf(stderr, ": %s\n", message);
    synchronize(parser);
}

void parser_error_at(Parser* parser, const Token* token, const char* message) {
    if (parser->panic_mode) return;
    parser->panic_mode = 1;
    parser->error_count++;
    fprintf(stderr, "[%s:%d:%d] Ошибка",
            parser->source_file ? parser->source_file : "<source>",
            token->line,
            token->column);
    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " в конце файла");
    } else if (token->type != TOKEN_ERROR) {
        char* text = token_copy_text(token);
        fprintf(stderr, " на токене '%.*s'", (int)token->length, text);
        FREE_ARRAY(char, text, token->length + 1);
    }
    fprintf(stderr, ": %s\n", message);
}

void parser_error_at_current(Parser* parser, const char* message) {
    parser_error_at(parser, &parser->current, message);
}

void parser_advance(Parser* parser) {
    parser->previous = parser->current;
    if (!parser->panic_mode) {
        while (1) {
            parser->current = lexer_next_token(parser->lexer);
            if (parser->current.type != TOKEN_ERROR) break;
            parser_error_at_current(parser, (const char*)parser->current.start);
        }
    } else {
        parser->current = lexer_next_token(parser->lexer);
    }
}

void parser_consume(Parser* parser, TokenType type, const char* message) {
    if (parser->current.type == type) {
        parser_advance(parser);
        return;
    }
    parser_error_at_current(parser, message);
}

int parser_check(Parser* parser, TokenType type) {
    return parser->current.type == type;
}

int parser_match(Parser* parser, TokenType type) {
    if (!parser_check(parser, type)) return 0;
    parser_advance(parser);
    return 1;
}

int parser_has_errors(const Parser* parser) {
    return parser->error_count > 0 || lexer_has_errors(parser->lexer);
}

int parser_error_count(const Parser* parser) {
    return parser->error_count + lexer_has_errors(parser->lexer);
}

ASTNode* parser_parse(Parser* parser) {
    ASTNode* statements = NULL;
    ASTNode* last_stmt = NULL;
    while (!parser_check(parser, TOKEN_EOF)) {
        ASTNode* decl = parse_declaration(parser);
        if (decl) {
            if (!statements) {
                statements = decl;
                last_stmt = decl;
            } else {
                last_stmt->next = decl;
                last_stmt = decl;
            }
        }
        if (parser->panic_mode) {
            while (!parser_check(parser, TOKEN_SEMICOLON) &&
                   !parser_check(parser, TOKEN_EOF)) {
                parser_advance(parser);
            }
            parser_match(parser, TOKEN_SEMICOLON);
        }
    }
    return ast_new_program(statements, 1, 1);
}

ASTNode* parse_declaration(Parser* parser) {
    if (parser_match(parser, TOKEN_LET)) {
        return parse_var_declaration(parser);
    }
    if (parser_match(parser, TOKEN_VAR)) {
        return parse_var_declaration(parser);
    }
    if (parser_match(parser, TOKEN_FN)) {
        return parse_function_statement(parser);
    }
    return parse_statement(parser);
}

ASTNode* parse_var_declaration(Parser* parser) {
    Token name = parser->current;
    parser_consume(parser, TOKEN_IDENTIFIER, "Ожидается имя переменной");
    ASTNode* initializer = NULL;
    if (parser_match(parser, TOKEN_EQUAL)) {
        initializer = parse_expression(parser);
    }

    consume_semicolon(parser);

    ASTNode* var_node = ast_new_variable((const char*)name.start, name.length, name.line, name.column);
    if (initializer) {
        return ast_new_assign(var_node, initializer, name.line, name.column);
    } else {
        return ast_new_expr_stmt(var_node, name.line, name.column);
    }
}

ASTNode* parse_statement(Parser* parser) {
    if (parser_match(parser, TOKEN_BREAK)) return parse_break_statement(parser);
    if (parser_match(parser, TOKEN_IF)) return parse_if_statement(parser);
    if (parser_match(parser, TOKEN_FOR)) return parse_for_statement(parser);
    if (parser_match(parser, TOKEN_RETURN)) return parse_return_statement(parser);
    if (parser_match(parser, TOKEN_WHILE)) return parse_while_statement(parser);
    if (parser_check(parser, TOKEN_LEFT_BRACE)) return parse_block_statement(parser);

    return parse_expression_statement(parser);
}

ASTNode* parse_expression_statement(Parser* parser) {
    ASTNode* expr = parse_expression(parser);
    consume_semicolon(parser);
    return ast_new_expr_stmt(expr, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
}

ASTNode* parse_if_statement(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;

    parser_consume(parser, TOKEN_LEFT_PAREN, "Ожидается '(' после 'if'");
    ASTNode* condition = parse_expression(parser);
    parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после условия");

    ASTNode* then_branch = parse_statement(parser);

    ASTNode* else_branch = NULL;
    if (parser_match(parser, TOKEN_ELSE)) {
        else_branch = parse_statement(parser);
    }

    return ast_new_if(condition, then_branch, else_branch, line, column);
}

ASTNode* parse_while_statement(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;
    parser->in_loop++;
    parser_consume(parser, TOKEN_LEFT_PAREN, "Ожидается '(' после 'while'");
    ASTNode* condition = parse_expression(parser);
    parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после условия");
    ASTNode* body = parse_statement(parser);
    parser->in_loop--;
    return ast_new_for(NULL, condition, NULL, body, line, column);
}

static ASTNode* parse_for_statement(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;

    parser->in_loop++;
    parser_consume(parser, TOKEN_LEFT_PAREN, "Ожидается '(' после 'for'");

    ASTNode* initializer = NULL;
    if (!parser_match(parser, TOKEN_SEMICOLON)) {
        if (parser_match(parser, TOKEN_LET) || parser_match(parser, TOKEN_VAR)) {
            initializer = parse_var_declaration_no_semi(parser);
            parser_consume(parser, TOKEN_SEMICOLON, "Ожидается ';' после инициализации for");
        } else {
            initializer = parse_expression(parser);
            parser_consume(parser, TOKEN_SEMICOLON, "Ожидается ';' после инициализации for");
        }
    }

    ASTNode* condition = NULL;
    if (!parser_check(parser, TOKEN_SEMICOLON)) {
        condition = parse_expression(parser);
    }

    parser_consume(parser, TOKEN_SEMICOLON, "Ожидается ';' после условия for");

    ASTNode* increment = NULL;
    if (!parser_check(parser, TOKEN_RIGHT_PAREN)) {
        increment = parse_expression(parser);
    }

    parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после инкремента for");

    ASTNode* body = parse_statement(parser);
    parser->in_loop--;

    return ast_new_for(initializer, condition, increment, body, line, column);
}

ASTNode* parse_break_statement(Parser* parser) {
    if (!parser->in_loop) {
        parser_error_at_current(parser, "'break' можно использовать только внутри цикла");
    }
    int line = parser->previous.line;
    int column = parser->previous.column;
    consume_semicolon(parser);
    return ast_new_break(line, column);
}

ASTNode* parse_block_statement(Parser* parser) {
    int line = parser->current.line;
    int column = parser->current.column;

    parser_consume(parser, TOKEN_LEFT_BRACE, "Ожидается '{'");

    ASTNode* statements = NULL;
    ASTNode* last_stmt = NULL;

    while (!parser_check(parser, TOKEN_RIGHT_BRACE) && !parser_check(parser, TOKEN_EOF)) {
        if (parser_match(parser, TOKEN_SEMICOLON)) {
            continue;
        }

        ASTNode* stmt = parse_declaration(parser);
        if (stmt) {
            if (!statements) {
                statements = stmt;
                last_stmt = stmt;
            } else {
                last_stmt->next = stmt;
                last_stmt = stmt;
            }
        }

        if (parser->panic_mode) {
            while (!parser_check(parser, TOKEN_SEMICOLON) &&
                   !parser_check(parser, TOKEN_RIGHT_BRACE) &&
                   !parser_check(parser, TOKEN_EOF)) {
                parser_advance(parser);
                   }
            parser_match(parser, TOKEN_SEMICOLON);
        }
    }

    parser_consume(parser, TOKEN_RIGHT_BRACE, "Ожидается '}' после блока");
    return ast_new_block(statements, line, column);
}

ASTNode* parse_block_statement_from_brace(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;
    ASTNode* statements = NULL;
    ASTNode* last_stmt = NULL;
    while (!parser_check(parser, TOKEN_RIGHT_BRACE) && !parser_check(parser, TOKEN_EOF)) {
        if (parser_match(parser, TOKEN_SEMICOLON)) {
            continue;
        }

        ASTNode* stmt = parse_declaration(parser);
        if (stmt) {
            if (!statements) {
                statements = stmt;
                last_stmt = stmt;
            } else {
                last_stmt->next = stmt;
                last_stmt = stmt;
            }
        }
        if (parser->panic_mode) {
            while (!parser_check(parser, TOKEN_SEMICOLON) &&
                   !parser_check(parser, TOKEN_RIGHT_BRACE) &&
                   !parser_check(parser, TOKEN_EOF)) {
                parser_advance(parser);
            }
            parser_match(parser, TOKEN_SEMICOLON);
        }
    }
    parser_consume(parser, TOKEN_RIGHT_BRACE, "Ожидается '}' после блока");
    return ast_new_block(statements, line, column);
}

ASTNode* parse_function_statement(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;
    parser->in_function++;

    Token name = parser->current;
    parser_consume(parser, TOKEN_IDENTIFIER, "Ожидается имя функции");

    parser_consume(parser, TOKEN_LEFT_PAREN, "Ожидается '(' после имени функции");

    char** params = NULL;
    size_t param_count = 0;
    if (!parser_check(parser, TOKEN_RIGHT_PAREN)) {
        parse_parameter_list(parser, &params, &param_count);
    }
    parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после параметров");

    ASTNode* body = parse_block_statement(parser);

    parser->in_function--;
    return ast_new_function((const char*)name.start, name.length, params, param_count, body, line, column);
}

int parse_parameter_list(Parser* parser, char*** params, size_t* param_count) {
    *params = NULL;
    *param_count = 0;
    size_t capacity = 4;
    *params = ALLOCATE(char*, capacity);
    do {
        if (*param_count >= capacity) {
            capacity *= 2;
            *params = GROW_ARRAY(char*, *params, *param_count, capacity);
        }
        Token param = parser->current;
        parser_consume(parser, TOKEN_IDENTIFIER, "Ожидается имя параметра");
        (*params)[*param_count] = ALLOCATE(char, param.length + 1);
        memcpy((*params)[*param_count], param.start, param.length);
        (*params)[*param_count][param.length] = '\0';
        (*param_count)++;
    } while (parser_match(parser, TOKEN_COMMA));
    return 1;
}

ASTNode* parse_return_statement(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;

    if (!parser->in_function) {
        parser_error(parser, "'return' можно использовать только внутри функции");
    }

    ASTNode* value = NULL;
    if (!parser_check(parser, TOKEN_SEMICOLON) && !parser_check(parser, TOKEN_RIGHT_BRACE)) {
        value = parse_expression(parser);
    }

    if (!parser_check(parser, TOKEN_RIGHT_BRACE)) {
        consume_semicolon(parser);
    }

    return ast_new_return(value, line, column);
}

ASTNode* parse_expression(Parser* parser) {
    if (parser_match(parser, TOKEN_FN)) {
        return parse_anonymous_function(parser);
    }
    return parse_assignment(parser);
}

ASTNode* parse_anonymous_function(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;
    parser->in_function++;
    parser_consume(parser, TOKEN_LEFT_PAREN, "Ожидается '(' после 'fn'");
    char** params = NULL;
    size_t param_count = 0;
    if (!parser_check(parser, TOKEN_RIGHT_PAREN)) {
        parse_parameter_list(parser, &params, &param_count);
    }
    parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после параметров");
    parser_consume(parser, TOKEN_LEFT_BRACE, "Ожидается '{' перед телом функции");
    ASTNode* body = parse_block_statement_from_brace(parser);
    parser->in_function--;
    return ast_new_function(NULL, 0, params, param_count, body, line, column);
}

ASTNode* parse_assignment(Parser* parser) {
    ASTNode* expr = parse_or(parser);
    if (!expr) return NULL;

    if (parser_match(parser, TOKEN_QUESTION)) {
        ASTNode* then_expr = parse_expression(parser);
        parser_consume(parser, TOKEN_COLON, "Ожидается ':' в тернарном операторе");
        ASTNode* else_expr = parse_assignment(parser);
        return ast_new_ternary(expr, then_expr, else_expr, expr->line, expr->column);
    }

    if (parser_match(parser, TOKEN_EQUAL) ||
        parser_match(parser, TOKEN_PLUS_EQUAL) ||
        parser_match(parser, TOKEN_MINUS_EQUAL) ||
        parser_match(parser, TOKEN_STAR_EQUAL) ||
        parser_match(parser, TOKEN_SLASH_EQUAL) ||
        parser_match(parser, TOKEN_PERCENT_EQUAL)) {

        TokenType operator = parser->previous.type;
        ASTNode* value = parse_assignment(parser);

        if (expr->type == NODE_VARIABLE_EXPR || expr->type == NODE_INDEX_EXPR) {
            if (operator == TOKEN_EQUAL) {
                return ast_new_assign(expr, value, expr->line, expr->column);
            }

            TokenType binary_operator;
            switch (operator) {
                case TOKEN_PLUS_EQUAL: binary_operator = TOKEN_PLUS; break;
                case TOKEN_MINUS_EQUAL: binary_operator = TOKEN_MINUS; break;
                case TOKEN_STAR_EQUAL: binary_operator = TOKEN_STAR; break;
                case TOKEN_SLASH_EQUAL: binary_operator = TOKEN_SLASH; break;
                case TOKEN_PERCENT_EQUAL: binary_operator = TOKEN_PERCENT; break;
                default: binary_operator = TOKEN_EQUAL;
            }

            ASTNode* binary_expr = ast_new_binary(expr, value, binary_operator, expr->line, expr->column);
            return ast_new_assign(expr, binary_expr, expr->line, expr->column);
        }

        parser_error_at_current(parser, "Недопустимый левый операнд для присваивания");
        return expr;
        }

    return expr;
}

ASTNode* parse_or(Parser* parser) {
    ASTNode* expr = parse_and(parser);
    while (parser_match(parser, TOKEN_OR)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_and(parser);
        expr = ast_new_logical(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_and(Parser* parser) {
    ASTNode* expr = parse_equality(parser);
    while (parser_match(parser, TOKEN_AND)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_equality(parser);
        expr = ast_new_logical(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_equality(Parser* parser) {
    ASTNode* expr = parse_comparison(parser);
    while (parser_match(parser, TOKEN_BANG_EQUAL) || parser_match(parser, TOKEN_EQUAL_EQUAL)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_comparison(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_comparison(Parser* parser) {
    ASTNode* expr = parse_bitwise_or(parser);
    while (parser_match(parser, TOKEN_GREATER) || parser_match(parser, TOKEN_GREATER_EQUAL) ||
           parser_match(parser, TOKEN_LESS) || parser_match(parser, TOKEN_LESS_EQUAL)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_bitwise_or(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_bitwise_or(Parser* parser) {
    ASTNode* expr = parse_bitwise_xor(parser);
    while (parser_match(parser, TOKEN_BIT_OR)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_bitwise_xor(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_bitwise_xor(Parser* parser) {
    ASTNode* expr = parse_bitwise_and(parser);
    while (parser_match(parser, TOKEN_BIT_XOR)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_bitwise_and(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_bitwise_and(Parser* parser) {
    ASTNode* expr = parse_shift(parser);
    while (parser_match(parser, TOKEN_BIT_AND)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_shift(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_shift(Parser* parser) {
    ASTNode* expr = parse_term(parser);
    while (parser_match(parser, TOKEN_BIT_LEFT_SHIFT) || parser_match(parser, TOKEN_BIT_RIGHT_SHIFT)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_term(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_term(Parser* parser) {
    ASTNode* expr = parse_factor(parser);
    while (parser_match(parser, TOKEN_PLUS) || parser_match(parser, TOKEN_MINUS)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_factor(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_factor(Parser* parser) {
    ASTNode* expr = parse_unary(parser);
    while (parser_match(parser, TOKEN_STAR) || parser_match(parser, TOKEN_SLASH) || parser_match(parser, TOKEN_PERCENT)) {
        TokenType operator = parser->previous.type;
        ASTNode* right = parse_unary(parser);
        expr = ast_new_binary(expr, right, operator, expr ? expr->line : parser->previous.line, expr ? expr->column : parser->previous.column);
    }
    return expr;
}

ASTNode* parse_unary(Parser* parser) {
    if (parser_match(parser, TOKEN_PLUS_PLUS) ||
        parser_match(parser, TOKEN_MINUS_MINUS) ||
        parser_match(parser, TOKEN_BANG) ||
        parser_match(parser, TOKEN_MINUS) ||
        parser_match(parser, TOKEN_BIT_NOT)) {
        TokenType operator = parser->previous.type;
        int line = parser->previous.line;
        int column = parser->previous.column;
        ASTNode* right = parse_unary(parser);
        return ast_new_unary(right, operator, line, column);
    }
    return parse_postfix(parser);
}

static ASTNode* parse_call(Parser* parser, ASTNode* callee) {
    int line = parser->previous.line;
    int column = parser->previous.column;

    ASTNode** args = NULL;
    size_t arg_count = 0;

    if (!parser_check(parser, TOKEN_RIGHT_PAREN)) {
        args = parse_argument_list(parser, &arg_count);
    }

    parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после аргументов");

    return ast_new_call(callee, args, arg_count, line, column);
}

ASTNode* parse_postfix(Parser* parser) {
    ASTNode* expr = parse_primary(parser);

    while (1) {
        if (parser_match(parser, TOKEN_PLUS_PLUS) || parser_match(parser, TOKEN_MINUS_MINUS)) {
            TokenType operator = parser->previous.type;
            int line = parser->previous.line;
            int column = parser->previous.column;
            expr = ast_new_postfix(expr, operator, line, column);
        }
        else if (parser_match(parser, TOKEN_LEFT_PAREN)) {
            expr = parse_call(parser, expr);
        }
        else if (parser_match(parser, TOKEN_LEFT_BRACKET)) {
            expr = parse_index(parser, expr);
        }
        else if (parser_match(parser, TOKEN_DOT)) {
            Token member = parser->current;
            parser_consume(parser, TOKEN_IDENTIFIER, "Ожидается имя после '.'");

            if (parser_check(parser, TOKEN_LEFT_PAREN)) {
                parser_match(parser, TOKEN_LEFT_PAREN);

                ASTNode** args = ALLOCATE(ASTNode*, 8);
                size_t arg_count = 0;
                size_t capacity = 8;

                args[arg_count++] = expr;

                if (!parser_check(parser, TOKEN_RIGHT_PAREN)) {
                    do {
                        if (arg_count >= capacity) {
                            capacity *= 2;
                            args = GROW_ARRAY(ASTNode*, args, arg_count, capacity);
                        }
                        args[arg_count] = parse_expression(parser);
                        arg_count++;
                    } while (parser_match(parser, TOKEN_COMMA));
                }

                parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после аргументов");

                ASTNode* callee = ast_new_variable((const char*)member.start, member.length, member.line, member.column);
                expr = ast_new_call(callee, args, arg_count, member.line, member.column);
            } else {
                parser_error_at_current(parser, "Доступ к свойствам без вызова не поддерживается");
            }
        }
        else {
            break;
        }
    }

    return expr;
}

ASTNode* parse_primary(Parser* parser) {
    if (parser_match(parser, TOKEN_FALSE)) {
        return ast_new_bool_literal(0, parser->previous.line, parser->previous.column);
    }
    if (parser_match(parser, TOKEN_TRUE)) {
        return ast_new_bool_literal(1, parser->previous.line, parser->previous.column);
    }
    if (parser_match(parser, TOKEN_NIL)) {
        return ast_new_nil_literal(parser->previous.line, parser->previous.column);
    }
    if (parser_match(parser, TOKEN_NUMBER)) {
        double value = token_as_number(&parser->previous);
        return ast_new_number_literal(value, parser->previous.line, parser->previous.column);
    }
    if (parser_match(parser, TOKEN_STRING)) {
        char* text = token_copy_text(&parser->previous);
        ASTNode* node = ast_new_string_literal(text, parser->previous.line, parser->previous.column);
        FREE_ARRAY(char, text, parser->previous.length + 1);
        return node;
    }
    if (parser_match(parser, TOKEN_IDENTIFIER)) {
        Token token = parser->previous;
        return ast_new_variable((const char*)token.start, token.length, token.line, token.column);
    }
    if (parser_match(parser, TOKEN_LEFT_PAREN)) {
        ASTNode* expr = parse_expression(parser);
        parser_consume(parser, TOKEN_RIGHT_PAREN, "Ожидается ')' после выражения");
        ASTNode* node = ast_new_node(NODE_GROUP_EXPR, parser->previous.line, parser->previous.column);
        node->unary.operand = expr;
        return node;
    }
    if (parser_match(parser, TOKEN_LEFT_BRACKET)) {
        return parse_array_literal(parser);
    }
    parser_error_at_current(parser, "Ожидается выражение");
    return NULL;
}

static ASTNode** parse_argument_list(Parser* parser, size_t* arg_count) {
    ASTNode** args = ALLOCATE(ASTNode*, 8);
    size_t capacity = 8;
    *arg_count = 0;
    do {
        if (*arg_count >= capacity) {
            capacity *= 2;
            args = GROW_ARRAY(ASTNode*, args, *arg_count, capacity);
        }
        args[*arg_count] = parse_expression(parser);
        (*arg_count)++;
    } while (parser_match(parser, TOKEN_COMMA));
    return args;
}

ASTNode* parse_array_literal(Parser* parser) {
    int line = parser->previous.line;
    int column = parser->previous.column;
    ASTNode** elements = NULL;
    size_t element_count = 0;
    size_t capacity = 4;
    elements = ALLOCATE(ASTNode*, capacity);
    if (!parser_check(parser, TOKEN_RIGHT_BRACKET)) {
        do {
            if (element_count >= capacity) {
                capacity *= 2;
                elements = GROW_ARRAY(ASTNode*, elements, element_count, capacity);
            }
            elements[element_count] = parse_expression(parser);
            element_count++;
        } while (parser_match(parser, TOKEN_COMMA));
    }
    parser_consume(parser, TOKEN_RIGHT_BRACKET, "Ожидается ']' после элементов массива");
    return ast_new_array(elements, element_count, line, column);
}

ASTNode* parse_index(Parser* parser, ASTNode* array) {
    int line = parser->previous.line;
    int column = parser->previous.column;
    ASTNode* index = parse_expression(parser);
    parser_consume(parser, TOKEN_RIGHT_BRACKET, "Ожидается ']' после индекса");
    return ast_new_index(array, index, line, column);
}
static ASTNode* parse_var_declaration_no_semi(Parser* parser) {
    Token name = parser->current;
    parser_consume(parser, TOKEN_IDENTIFIER, "Ожидается имя переменной");
    ASTNode* initializer = NULL;
    if (parser_match(parser, TOKEN_EQUAL)) {
        initializer = parse_expression(parser);
    }

    ASTNode* var_node = ast_new_variable((const char*)name.start, name.length, name.line, name.column);
    if (initializer) {
        return ast_new_assign(var_node, initializer, name.line, name.column);
    } else {
        return ast_new_expr_stmt(var_node, name.line, name.column);
    }
}

static void consume_semicolon(Parser* parser) {
    if (parser_match(parser, TOKEN_SEMICOLON)) {
        return;
    }

    if (parser_check(parser, TOKEN_RIGHT_BRACE) ||
        parser_check(parser, TOKEN_EOF) ||
        parser_check(parser, TOKEN_ELSE)) {
        return;
    }

    if (parser->previous.line != parser->current.line) {
        return;
    }

    parser_error_at_current(parser, "Ожидается ';' или новая строка");
}
