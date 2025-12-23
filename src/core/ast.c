#include "ast.h"
#include "memory.h"
#include "utils.h"
#include <stdio.h>
#include <string.h>

ASTNode* ast_new_node(NodeType type, int line, int column) {
    ASTNode* node = ALLOCATE(ASTNode, 1);
    node->type = type;
    node->line = line;
    node->column = column;
    node->next = NULL;
    return node;
}

ASTNode* ast_new_binary(ASTNode* left, ASTNode* right, TokenType operator, int line, int column) {
    ASTNode* node = ast_new_node(NODE_BINARY_EXPR, line, column);
    node->binary.left = left;
    node->binary.right = right;
    node->binary.operator = operator;
    return node;
}

ASTNode* ast_new_ternary(ASTNode* condition, ASTNode* then_expr, ASTNode* else_expr, int line, int column) {
    ASTNode* node = ast_new_node(NODE_TERNARY_EXPR, line, column);
    node->ternary.condition = condition;
    node->ternary.then_expr = then_expr;
    node->ternary.else_expr = else_expr;
    return node;
}

ASTNode* ast_new_unary(ASTNode* operand, TokenType operator, int line, int column) {
    ASTNode* node = ast_new_node(NODE_UNARY_EXPR, line, column);
    node->unary.operand = operand;
    node->unary.operator = operator;
    return node;
}

ASTNode* ast_new_postfix(ASTNode* operand, TokenType operator, int line, int column) {
    ASTNode* node = ast_new_node(NODE_POSTFIX_EXPR, line, column);
    node->postfix.operand = operand;
    node->postfix.operator = operator;
    return node;
}

ASTNode* ast_new_number_literal(double value, int line, int column) {
    ASTNode* node = ast_new_node(NODE_LITERAL_EXPR, line, column);
    node->literal.value_type = LIT_NUMBER;
    node->literal.value.number = value;
    return node;
}

ASTNode* ast_new_string_literal(const char* value, int line, int column) {
    ASTNode* node = ast_new_node(NODE_LITERAL_EXPR, line, column);
    node->literal.value_type = LIT_STRING;
    node->literal.value.string = copy_string(value);
    return node;
}

ASTNode* ast_new_bool_literal(int value, int line, int column) {
    ASTNode* node = ast_new_node(NODE_LITERAL_EXPR, line, column);
    node->literal.value_type = LIT_BOOL;
    node->literal.value.boolean = value;
    return node;
}

ASTNode* ast_new_nil_literal(int line, int column) {
    ASTNode* node = ast_new_node(NODE_LITERAL_EXPR, line, column);
    node->literal.value_type = LIT_NIL;
    return node;
}

ASTNode* ast_new_variable(const char* name, size_t length, int line, int column) {
    ASTNode* node = ast_new_node(NODE_VARIABLE_EXPR, line, column);
    node->variable.name = ALLOCATE(char, length + 1);
    memcpy(node->variable.name, name, length);
    node->variable.name[length] = '\0';
    node->variable.name_length = length;
    return node;
}

ASTNode* ast_new_assign(ASTNode* target, ASTNode* value, int line, int column) {
    ASTNode* node = ast_new_node(NODE_ASSIGN_EXPR, line, column);
    node->assign.target = target;
    node->assign.value = value;
    return node;
}

ASTNode* ast_new_call(ASTNode* callee, ASTNode** args, size_t arg_count, int line, int column) {
    ASTNode* node = ast_new_node(NODE_CALL_EXPR, line, column);
    node->call.callee = callee;
    node->call.arguments = args;
    node->call.arg_count = arg_count;
    return node;
}

ASTNode* ast_new_block(ASTNode* statements, int line, int column) {
    ASTNode* node = ast_new_node(NODE_BLOCK_STMT, line, column);
    node->block.statements = statements;
    return node;
}

ASTNode* ast_new_if(ASTNode* condition, ASTNode* then_branch, ASTNode* else_branch, int line, int column) {
    ASTNode* node = ast_new_node(NODE_IF_STMT, line, column);
    node->if_stmt.condition = condition;
    node->if_stmt.then_branch = then_branch;
    node->if_stmt.else_branch = else_branch;
    return node;
}

ASTNode* ast_new_for(ASTNode* init, ASTNode* condition, ASTNode* increment, ASTNode* body, int line, int column) {
    ASTNode* node = ast_new_node(NODE_FOR_STMT, line, column);
    node->for_stmt.initializer = init;
    node->for_stmt.condition = condition;
    node->for_stmt.increment = increment;
    node->for_stmt.body = body;
    return node;
}

ASTNode* ast_new_function(const char* name, size_t length, char** params, size_t param_count, ASTNode* body, int line, int column) {
    ASTNode* node = ast_new_node(NODE_FUNCTION_STMT, line, column);

    node->function_stmt.name = ALLOCATE(char, length + 1);
    memcpy(node->function_stmt.name, name, length);
    node->function_stmt.name[length] = '\0';
    node->function_stmt.name_length = length;

    node->function_stmt.parameters = params;
    node->function_stmt.param_count = param_count;
    node->function_stmt.body = body;

    return node;
}

ASTNode* ast_new_return(ASTNode* value, int line, int column) {
    ASTNode* node = ast_new_node(NODE_RETURN_STMT, line, column);
    node->return_stmt.value = value;
    return node;
}

ASTNode* ast_new_program(ASTNode* statements, int line, int column) {
    ASTNode* node = ast_new_node(NODE_PROGRAM, line, column);
    node->program.statements = statements;
    return node;
}

ASTNode* ast_new_expr_stmt(ASTNode* expression, int line, int column) {
    ASTNode* node = ast_new_node(NODE_EXPR_STMT, line, column);
    node->expr_stmt.expression = expression;
    return node;
}

ASTNode* ast_new_array(ASTNode** elements, size_t count, int line, int column) {
    ASTNode* node = ast_new_node(NODE_ARRAY_EXPR, line, column);
    node->array.elements = elements;
    node->array.element_count = count;
    return node;
}

ASTNode* ast_new_index(ASTNode* array, ASTNode* index, int line, int column) {
    ASTNode* node = ast_new_node(NODE_INDEX_EXPR, line, column);
    node->index.array = array;
    node->index.index = index;
    return node;
}

ASTNode* ast_new_logical(ASTNode* left, ASTNode* right, TokenType operator, int line, int column) {
    ASTNode* node = ast_new_node(NODE_LOGICAL_EXPR, line, column);
    node->logical.left = left;
    node->logical.right = right;
    node->logical.operator = operator;
    return node;
}

ASTNode* ast_new_break(int line, int column) {
    ASTNode* node = ast_new_node(NODE_BREAK_STMT, line, column);
    return node;
}

ASTNode* ast_new_continue(ASTNode* target, ASTNode* value, int line, int column) {
    ASTNode* node = ast_new_node(NODE_CONTINUE_STMT, line, column);
    node->continue_stmt.target = target;
    node->continue_stmt.value = value;
    return node;
}

ASTNode* ast_new_group(ASTNode* expression, int line, int column) {
    ASTNode* node = ast_new_node(NODE_GROUP_EXPR, line, column);
    node->unary.operand = expression;
    return node;
}

ASTNode* ast_new_member(ASTNode* object, const char* name, size_t length, int line, int column) {
    ASTNode* node = ast_new_node(NODE_MEMBER_EXPR, line, column);
    node->member.object = object;
    node->member.name = ALLOCATE(char, length + 1);
    memcpy(node->member.name, name, length);
    node->member.name[length] = '\0';
    node->member.name_length = length;
    return node;
}

void ast_free(ASTNode* node) {
    if (!node) return;

    switch (node->type) {
        case NODE_BINARY_EXPR:
            ast_free(node->binary.left);
            ast_free(node->binary.right);
            break;

        case NODE_TERNARY_EXPR:
            ast_free(node->ternary.condition);
            ast_free(node->ternary.then_expr);
            ast_free(node->ternary.else_expr);
            break;

        case NODE_UNARY_EXPR:
        case NODE_POSTFIX_EXPR:
        case NODE_GROUP_EXPR:
            ast_free(node->unary.operand);
            break;

        case NODE_LITERAL_EXPR:
            if (node->literal.value_type == LIT_STRING && node->literal.value.string) {
                FREE_ARRAY(char, node->literal.value.string, strlen(node->literal.value.string) + 1);
            }
            break;

        case NODE_VARIABLE_EXPR:
            if (node->variable.name) {
                FREE_ARRAY(char, node->variable.name, node->variable.name_length + 1);
            }
            break;

        case NODE_ASSIGN_EXPR:
            ast_free(node->assign.target);
            ast_free(node->assign.value);
            break;

        case NODE_CALL_EXPR:
            ast_free(node->call.callee);
            ast_free_array(node->call.arguments, node->call.arg_count);
            break;

        case NODE_ARRAY_EXPR:
            ast_free_array(node->array.elements, node->array.element_count);
            break;

        case NODE_INDEX_EXPR:
            ast_free(node->index.array);
            ast_free(node->index.index);
            break;

        case NODE_LOGICAL_EXPR:
            ast_free(node->logical.left);
            ast_free(node->logical.right);
            break;

        case NODE_MEMBER_EXPR:
            ast_free(node->member.object);
            if (node->member.name) {
                FREE_ARRAY(char, node->member.name, node->member.name_length + 1);
            }
            break;

        case NODE_EXPR_STMT:
            ast_free(node->expr_stmt.expression);
            break;

        case NODE_BLOCK_STMT:
            ast_free(node->block.statements);
            break;

        case NODE_IF_STMT:
            ast_free(node->if_stmt.condition);
            ast_free(node->if_stmt.then_branch);
            if (node->if_stmt.else_branch) {
                ast_free(node->if_stmt.else_branch);
            }
            break;

        case NODE_FOR_STMT:
            if (node->for_stmt.initializer) ast_free(node->for_stmt.initializer);
            if (node->for_stmt.condition) ast_free(node->for_stmt.condition);
            if (node->for_stmt.increment) ast_free(node->for_stmt.increment);
            if (node->for_stmt.body) ast_free(node->for_stmt.body);
            break;

        case NODE_FUNCTION_STMT:
            if (node->function_stmt.name) {
                FREE_ARRAY(char, node->function_stmt.name, node->function_stmt.name_length + 1);
            }
            if (node->function_stmt.parameters) {
                for (size_t i = 0; i < node->function_stmt.param_count; i++) {
                    if (node->function_stmt.parameters[i]) {
                        FREE_ARRAY(char, node->function_stmt.parameters[i],
                                   strlen(node->function_stmt.parameters[i]) + 1);
                    }
                }
                FREE_ARRAY(char*, node->function_stmt.parameters, node->function_stmt.param_count);
            }
            ast_free(node->function_stmt.body);
            break;

        case NODE_RETURN_STMT:
            if (node->return_stmt.value) {
                ast_free(node->return_stmt.value);
            }
            break;

        case NODE_BREAK_STMT:
        case NODE_CONTINUE_STMT:
            break;

        case NODE_PROGRAM:
            ast_free(node->program.statements);
            break;

        default:
            break;
    }

    ast_free(node->next);
    FREE_ARRAY(ASTNode, node, 1);
}

void ast_free_array(ASTNode** nodes, size_t count) {
    if (!nodes) return;
    for (size_t i = 0; i < count; i++) {
        ast_free(nodes[i]);
    }
    FREE_ARRAY(ASTNode*, nodes, count);
}

static void print_indent(int indent) {
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
}

static const char* token_type_to_string_simple(TokenType type) {
    switch (type) {
        case TOKEN_PLUS: return "+";
        case TOKEN_MINUS: return "-";
        case TOKEN_STAR: return "*";
        case TOKEN_SLASH: return "/";
        case TOKEN_PERCENT: return "%";
        case TOKEN_PLUS_PLUS: return "++";
        case TOKEN_MINUS_MINUS: return "--";
        case TOKEN_BANG: return "!";
        case TOKEN_BANG_EQUAL: return "!=";
        case TOKEN_EQUAL: return "=";
        case TOKEN_EQUAL_EQUAL: return "==";
        case TOKEN_GREATER: return ">";
        case TOKEN_GREATER_EQUAL: return ">=";
        case TOKEN_LESS: return "<";
        case TOKEN_LESS_EQUAL: return "<=";
        case TOKEN_AND: return "and";
        case TOKEN_OR: return "or";
        case TOKEN_BIT_AND: return "&";
        case TOKEN_BIT_OR: return "|";
        case TOKEN_BIT_XOR: return "^";
        case TOKEN_BIT_LEFT_SHIFT: return "<<";
        case TOKEN_BIT_RIGHT_SHIFT: return ">>";
        case TOKEN_BIT_NOT: return "~";

        case TOKEN_PLUS_EQUAL: return "+=";
        case TOKEN_MINUS_EQUAL: return "-=";
        case TOKEN_STAR_EQUAL: return "*=";
        case TOKEN_SLASH_EQUAL: return "/=";
        case TOKEN_PERCENT_EQUAL: return "%=";
        case TOKEN_BIT_AND_EQUAL: return "&=";
        case TOKEN_BIT_OR_EQUAL: return "|=";
        case TOKEN_BIT_XOR_EQUAL: return "^=";
        case TOKEN_BIT_LEFT_SHIFT_EQUAL: return "<<=";
        case TOKEN_BIT_RIGHT_SHIFT_EQUAL: return ">>=";

        case TOKEN_QUESTION: return "?";
        case TOKEN_COLON: return ":";
        case TOKEN_COMMA: return ",";
        case TOKEN_SEMICOLON: return ";";
        case TOKEN_DOT: return ".";
        case TOKEN_LEFT_PAREN: return "(";
        case TOKEN_RIGHT_PAREN: return ")";
        case TOKEN_LEFT_BRACE: return "{";
        case TOKEN_RIGHT_BRACE: return "}";
        case TOKEN_LEFT_BRACKET: return "[";
        case TOKEN_RIGHT_BRACKET: return "]";

        default: return "?";
    }
}

void ast_print(const ASTNode* node, int indent) {
    if (!node) return;

    print_indent(indent);

    switch (node->type) {
        case NODE_BINARY_EXPR:
            printf("BinaryExpr(%s) at %d:%d\n",
                   token_type_to_string_simple(node->binary.operator),
                   node->line, node->column);
            ast_print(node->binary.left, indent + 1);
            ast_print(node->binary.right, indent + 1);
            break;

        case NODE_TERNARY_EXPR:
            printf("Ternary at %d:%d\n", node->line, node->column);
            print_indent(indent + 1);
            printf("Condition:\n");
            ast_print(node->ternary.condition, indent + 2);
            print_indent(indent + 1);
            printf("Then:\n");
            ast_print(node->ternary.then_expr, indent + 2);
            print_indent(indent + 1);
            printf("Else:\n");
            ast_print(node->ternary.else_expr, indent + 2);
            break;

        case NODE_UNARY_EXPR:
            printf("UnaryExpr(%s) at %d:%d\n",
                   token_type_to_string_simple(node->unary.operator),
                   node->line, node->column);
            ast_print(node->unary.operand, indent + 1);
            break;

        case NODE_POSTFIX_EXPR:
            printf("PostfixExpr(%s) at %d:%d\n",
                   token_type_to_string_simple(node->postfix.operator),
                   node->line, node->column);
            ast_print(node->postfix.operand, indent + 1);
            break;

        case NODE_LITERAL_EXPR:
            switch (node->literal.value_type) {
                case LIT_NUMBER:
                    printf("Literal(number=%.6g) at %d:%d\n",
                           node->literal.value.number, node->line, node->column);
                    break;
                case LIT_STRING:
                    printf("Literal(string=\"%s\") at %d:%d\n",
                           node->literal.value.string, node->line, node->column);
                    break;
                case LIT_BOOL:
                    printf("Literal(bool=%s) at %d:%d\n",
                           node->literal.value.boolean ? "true" : "false",
                           node->line, node->column);
                    break;
                case LIT_NIL:
                    printf("Literal(nil) at %d:%d\n", node->line, node->column);
                    break;
            }
            break;

        case NODE_VARIABLE_EXPR:
            printf("Variable(name=%s) at %d:%d\n",
                   node->variable.name, node->line, node->column);
            break;

        case NODE_ASSIGN_EXPR:
            printf("Assign at %d:%d\n", node->line, node->column);
            ast_print(node->assign.target, indent + 1);
            ast_print(node->assign.value, indent + 1);
            break;

        case NODE_CALL_EXPR:
            printf("Call(arg_count=%zu) at %d:%d\n",
                   node->call.arg_count, node->line, node->column);
            ast_print(node->call.callee, indent + 1);
            for (size_t i = 0; i < node->call.arg_count; i++) {
                ast_print(node->call.arguments[i], indent + 1);
            }
            break;

        case NODE_ARRAY_EXPR:
            printf("Array(element_count=%zu) at %d:%d\n",
                   node->array.element_count, node->line, node->column);
            for (size_t i = 0; i < node->array.element_count; i++) {
                ast_print(node->array.elements[i], indent + 1);
            }
            break;

        case NODE_INDEX_EXPR:
            printf("Index at %d:%d\n", node->line, node->column);
            ast_print(node->index.array, indent + 1);
            ast_print(node->index.index, indent + 1);
            break;

        case NODE_LOGICAL_EXPR:
            printf("Logical(%s) at %d:%d\n",
                   token_type_to_string_simple(node->logical.operator),
                   node->line, node->column);
            ast_print(node->logical.left, indent + 1);
            ast_print(node->logical.right, indent + 1);
            break;

        case NODE_MEMBER_EXPR:
            printf("Member(%s) at %d:%d\n", node->member.name, node->line, node->column);
            ast_print(node->member.object, indent + 1);
            break;

        case NODE_GROUP_EXPR:
            printf("Group at %d:%d\n", node->line, node->column);
            ast_print(node->unary.operand, indent + 1);
            break;

        case NODE_EXPR_STMT:
            printf("ExprStmt at %d:%d\n", node->line, node->column);
            ast_print(node->expr_stmt.expression, indent + 1);
            break;

        case NODE_BLOCK_STMT:
            printf("Block at %d:%d\n", node->line, node->column);
            ast_print(node->block.statements, indent + 1);
            break;

        case NODE_IF_STMT:
            printf("If at %d:%d\n", node->line, node->column);
            ast_print(node->if_stmt.condition, indent + 1);
            printf("Then:\n");
            ast_print(node->if_stmt.then_branch, indent + 1);
            if (node->if_stmt.else_branch) {
                printf("Else:\n");
                ast_print(node->if_stmt.else_branch, indent + 1);
            }
            break;

        case NODE_FOR_STMT:
            printf("For at %d:%d\n", node->line, node->column);
            if (node->for_stmt.initializer) {
                printf("Init:\n");
                ast_print(node->for_stmt.initializer, indent + 1);
            }
            if (node->for_stmt.condition) {
                printf("Condition:\n");
                ast_print(node->for_stmt.condition, indent + 1);
            }
            if (node->for_stmt.increment) {
                printf("Increment:\n");
                ast_print(node->for_stmt.increment, indent + 1);
            }
            printf("Body:\n");
            ast_print(node->for_stmt.body, indent + 1);
            break;

        case NODE_FUNCTION_STMT:
            printf("Function(name=%s, params=%zu) at %d:%d\n",
                   node->function_stmt.name ? node->function_stmt.name : "<anonymous>",
                   node->function_stmt.param_count, node->line, node->column);
            if (node->function_stmt.parameters) {
                for (size_t i = 0; i < node->function_stmt.param_count; i++) {
                    print_indent(indent + 1);
                    printf("Param: %s\n", node->function_stmt.parameters[i]);
                }
            }
            ast_print(node->function_stmt.body, indent + 1);
            break;

        case NODE_RETURN_STMT:
            printf("Return at %d:%d\n", node->line, node->column);
            if (node->return_stmt.value) {
                ast_print(node->return_stmt.value, indent + 1);
            }
            break;

        case NODE_BREAK_STMT:
            printf("Break at %d:%d\n", node->line, node->column);
            break;

        case NODE_CONTINUE_STMT:
            printf("Continue at %d:%d\n", node->line, node->column);
            break;

        case NODE_PROGRAM:
            printf("Program at %d:%d\n", node->line, node->column);
            ast_print(node->program.statements, indent + 1);
            break;

        default:
            printf("Unknown node type: %d\n", node->type);
            break;
    }

    if (node->next) {
        ast_print(node->next, indent);
    }
}

const char* node_type_to_string(NodeType type) {
    switch (type) {
        case NODE_BINARY_EXPR: return "BinaryExpr";
        case NODE_TERNARY_EXPR: return "TernaryExpr";
        case NODE_UNARY_EXPR: return "UnaryExpr";
        case NODE_POSTFIX_EXPR: return "PostfixExpr";
        case NODE_LITERAL_EXPR: return "LiteralExpr";
        case NODE_VARIABLE_EXPR: return "VariableExpr";
        case NODE_ASSIGN_EXPR: return "AssignExpr";
        case NODE_CALL_EXPR: return "CallExpr";
        case NODE_ARRAY_EXPR: return "ArrayExpr";
        case NODE_INDEX_EXPR: return "IndexExpr";
        case NODE_LOGICAL_EXPR: return "LogicalExpr";
        case NODE_MEMBER_EXPR: return "MemberExpr";
        case NODE_GROUP_EXPR: return "GroupExpr";
        case NODE_EXPR_STMT: return "ExprStmt";
        case NODE_BLOCK_STMT: return "BlockStmt";
        case NODE_IF_STMT: return "IfStmt";
        case NODE_FOR_STMT: return "ForStmt";
        case NODE_FUNCTION_STMT: return "FunctionStmt";
        case NODE_RETURN_STMT: return "ReturnStmt";
        case NODE_BREAK_STMT: return "BreakStmt";
        case NODE_CONTINUE_STMT: return "ContinueStmt";
        case NODE_PROGRAM: return "Program";
        default: return "Unknown";
    }
}

int ast_node_is_statement(NodeType type) {
    return type >= NODE_EXPR_STMT;
}

int ast_node_is_expression(NodeType type) {
    return type < NODE_EXPR_STMT;
}

ASTNode* ast_last_statement(ASTNode* statements) {
    ASTNode* current = statements;
    while (current && current->next) {
        current = current->next;
    }
    return current;
}
