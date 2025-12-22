#include "compiler.h"
#include "memory.h"
#include "native.h"
#include <stdio.h>
#include <string.h>

#define INITIAL_LOCALS_CAPACITY 8
#define INITIAL_LOOP_JUMPS_CAPACITY 4

static void init_locals(Compiler* compiler) {
    compiler->locals.names = ALLOCATE(char*, INITIAL_LOCALS_CAPACITY);
    compiler->locals.depths = ALLOCATE(int, INITIAL_LOCALS_CAPACITY);
    compiler->locals.count = 0;
    compiler->locals.capacity = INITIAL_LOCALS_CAPACITY;
}

void free_locals(Compiler* compiler) {
    for (int i = 0; i < compiler->locals.count; i++) {
        if (compiler->locals.names[i]) {
            FREE_ARRAY(char, compiler->locals.names[i], strlen(compiler->locals.names[i]) + 1);
        }
    }
    FREE_ARRAY(char*, compiler->locals.names, compiler->locals.capacity);
    FREE_ARRAY(int, compiler->locals.depths, compiler->locals.capacity);
}

void compiler_free(Compiler* compiler) {
    if (!compiler) return;

    free_locals(compiler);

    if (compiler->loop.break_jumps) {
        FREE_ARRAY(size_t, compiler->loop.break_jumps, compiler->loop.break_capacity);
    }
    if (compiler->loop.continue_jumps) {
        FREE_ARRAY(size_t, compiler->loop.continue_jumps, compiler->loop.continue_capacity);
    }

    if (compiler->function.current_function) {
        FREE_ARRAY(char, compiler->function.current_function,
                   strlen(compiler->function.current_function) + 1);
    }
}

void compiler_init(Compiler* compiler, const char* source_file) {
    compiler->scope_depth = 0;
    compiler->error_count = 0;
    compiler->error_message = NULL;
    compiler->source_file = source_file;
    compiler->current_line = 1;

    compiler->function.current_function = NULL;
    compiler->function.function_depth = 0;
    compiler->function.return_count = 0;

    compiler->loop.loop_depth = 0;
    compiler->loop.break_jumps = NULL;
    compiler->loop.break_count = 0;
    compiler->loop.break_capacity = 0;
    compiler->loop.continue_jumps = NULL;
    compiler->loop.continue_count = 0;
    compiler->loop.continue_capacity = 0;

    init_locals(compiler);
}

static void init_loop_jumps(Compiler* compiler) {
    if (compiler->loop.break_capacity == 0) {
        compiler->loop.break_capacity = INITIAL_LOOP_JUMPS_CAPACITY;
        compiler->loop.break_jumps = ALLOCATE(size_t, compiler->loop.break_capacity);
    }
    if (compiler->loop.continue_capacity == 0) {
        compiler->loop.continue_capacity = INITIAL_LOOP_JUMPS_CAPACITY;
        compiler->loop.continue_jumps = ALLOCATE(size_t, compiler->loop.continue_capacity);
    }
}

CompileResult compiler_compile(Compiler* compiler, const ASTNode* ast, Bytecode* bytecode) {
    if (!ast) {
        compiler_error(compiler, "Empty AST");
        return COMPILE_ERROR;
    }

    return compile_node(compiler, ast, bytecode);
}

CompileResult compile_node(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (!node) {
        return COMPILE_SUCCESS;
    }

    compiler->current_line = node->line;

    switch (node->type) {
        case NODE_PROGRAM:
            return compile_program(compiler, node, bytecode);
        case NODE_BLOCK_STMT:
            return compile_block(compiler, node, bytecode);
        case NODE_EXPR_STMT:
            return compile_expression(compiler, node->expr_stmt.expression, bytecode);
        case NODE_IF_STMT:
            return compile_if(compiler, node, bytecode);
        case NODE_FOR_STMT:
            return compile_for(compiler, node, bytecode);
        case NODE_FUNCTION_STMT:
            return compile_function(compiler, node, bytecode);
        case NODE_RETURN_STMT:
            return compile_return(compiler, node, bytecode);
        case NODE_BREAK_STMT:
            return compile_break(compiler, bytecode);
        case NODE_CONTINUE_STMT:
            return compile_continue(compiler, bytecode);
        case NODE_GROUP_EXPR:
            return compile_expression(compiler, node->unary.operand, bytecode);
        case NODE_TERNARY_EXPR:
            return compile_ternary(compiler, node, bytecode);

        case NODE_BINARY_EXPR:
            return compile_binary(compiler, node, bytecode);
        case NODE_UNARY_EXPR:
            return compile_unary(compiler, node, bytecode);
        case NODE_POSTFIX_EXPR:
            return compile_postfix(compiler, node, bytecode);
        case NODE_LITERAL_EXPR:
            return compile_literal(compiler, node, bytecode);
        case NODE_VARIABLE_EXPR:
            return compile_variable(compiler, node, bytecode);
        case NODE_ASSIGN_EXPR:
            return compile_assign(compiler, node, bytecode);
        case NODE_CALL_EXPR:
            return compile_call(compiler, node, bytecode);
        case NODE_LOGICAL_EXPR:
            return compile_logical(compiler, node, bytecode);
        case NODE_ARRAY_EXPR:
            return compile_array(compiler, node, bytecode);
        case NODE_INDEX_EXPR:
            return compile_index(compiler, node, bytecode);
        case NODE_MEMBER_EXPR:
            return compile_member(compiler, node, bytecode);

        default:
            compiler_error_at_line(compiler, node->line, "Unknown AST node type");
            return COMPILE_ERROR;
    }
}

CompileResult compile_program(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    ASTNode* stmt = node->program.statements;
    CompileResult result = COMPILE_SUCCESS;

    while (stmt && result == COMPILE_SUCCESS) {
        result = compile_node(compiler, stmt, bytecode);
        stmt = stmt->next;
    }

    emit_op(compiler, bytecode, OP_HALT);
    return result;
}

CompileResult compile_block(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    int start_count = compiler->locals.count;
    compiler_begin_scope(compiler);

    ASTNode* stmt = node->block.statements;
    CompileResult result = COMPILE_SUCCESS;

    while (stmt && result == COMPILE_SUCCESS) {
        result = compile_node(compiler, stmt, bytecode);
        stmt = stmt->next;
    }

    compiler_end_scope(compiler, bytecode, start_count);
    return result;
}

CompileResult compile_expression(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    return compile_node(compiler, node, bytecode);
}

CompileResult compile_statement(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    return compile_node(compiler, node, bytecode);
}

CompileResult compile_binary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->binary.left, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    result = compile_expression(compiler, node->binary.right, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    OpCode op = get_binary_opcode(node->binary.operator);
    if (op == 0) {
        compiler_error_at_line(compiler, node->line, "Unknown binary operator");
        return COMPILE_ERROR;
    }

    emit_op(compiler, bytecode, op);
    return COMPILE_SUCCESS;
}

CompileResult compile_unary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->unary.operand, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    OpCode op = get_unary_opcode(node->unary.operator);
    if (op == 0) {
        compiler_error_at_line(compiler, node->line, "Unknown unary operator");
        return COMPILE_ERROR;
    }

    emit_op(compiler, bytecode, op);
    return COMPILE_SUCCESS;
}

CompileResult compile_postfix(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (node->postfix.operator != TOKEN_PLUS_PLUS && node->postfix.operator != TOKEN_MINUS_MINUS) {
        compiler_error_at_line(compiler, node->line, "Unknown postfix operator");
        return COMPILE_ERROR;
    }

    CompileResult result = compile_expression(compiler, node->postfix.operand, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    emit_op(compiler, bytecode, OP_DUP);

    if (node->postfix.operator == TOKEN_PLUS_PLUS) {
        emit_op(compiler, bytecode, OP_PUSH_I8);
        emit_i8(compiler, bytecode, 1);
        emit_op(compiler, bytecode, OP_ADD);
    } else {
        emit_op(compiler, bytecode, OP_PUSH_I8);
        emit_i8(compiler, bytecode, 1);
        emit_op(compiler, bytecode, OP_SUB);
    }

    if (node->postfix.operand->type == NODE_VARIABLE_EXPR) {
        int index = compiler_resolve_local(compiler,
                                           node->postfix.operand->variable.name,
                                           node->postfix.operand->variable.name_length);
        if (index >= 0) {
            if (index <= 255) {
                emit_op(compiler, bytecode, OP_STORE_LOCAL_8);
                emit_u8(compiler, bytecode, (uint8_t)index);
            } else {
                emit_op(compiler, bytecode, OP_STORE_LOCAL_16);
                emit_u16(compiler, bytecode, (uint16_t)index);
            }
        }
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_literal(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    switch (node->literal.value_type) {
        case LIT_NUMBER: {
            double value = node->literal.value.number;
            int64_t int_val = (int64_t)value;

            if (value == (double)int_val) {
                if (int_val >= -128 && int_val <= 127) {
                    emit_op(compiler, bytecode, OP_PUSH_I8);
                    emit_i8(compiler, bytecode, (int8_t)int_val);
                } else if (int_val >= -32768 && int_val <= 32767) {
                    emit_op(compiler, bytecode, OP_PUSH_I16);
                    emit_i16(compiler, bytecode, (int16_t)int_val);
                } else if (int_val >= -2147483648LL && int_val <= 2147483647LL) {
                    emit_op(compiler, bytecode, OP_PUSH_I32);
                    emit_i32(compiler, bytecode, (int32_t)int_val);
                } else {
                    int const_idx = bc_add_constant_int(bytecode, int_val);
                    emit_op(compiler, bytecode, OP_LOAD_CONST);
                    emit_u32(compiler, bytecode, (uint32_t)const_idx);
                }
            } else {
                int const_idx = bc_add_constant_float(bytecode, value);
                emit_op(compiler, bytecode, OP_LOAD_CONST);
                emit_u32(compiler, bytecode, (uint32_t)const_idx);
            }
            break;
        }
        case LIT_STRING: {
            int const_idx = bc_add_constant_string(bytecode, node->literal.value.string);
            emit_op(compiler, bytecode, OP_LOAD_CONST);
            emit_u32(compiler, bytecode, (uint32_t)const_idx);
            break;
        }
        case LIT_BOOL:
            if (node->literal.value.boolean) {
                emit_op(compiler, bytecode, OP_PUSH_TRUE);
            } else {
                emit_op(compiler, bytecode, OP_PUSH_FALSE);
            }
            break;
        case LIT_NIL:
            emit_op(compiler, bytecode, OP_PUSH_NIL);
            break;
    }
    return COMPILE_SUCCESS;
}

CompileResult compile_variable(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    int index = compiler_resolve_local(compiler, node->variable.name, node->variable.name_length);
    if (index < 0) {
        compiler_error_at_line(compiler, node->line, "Unknown variable");
        return COMPILE_ERROR;
    }

    if (index <= 255) {
        emit_op(compiler, bytecode, OP_LOAD_LOCAL_8);
        emit_u8(compiler, bytecode, (uint8_t)index);
    } else {
        emit_op(compiler, bytecode, OP_LOAD_LOCAL_16);
        emit_u16(compiler, bytecode, (uint16_t)index);
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_assign(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->assign.value, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    if (node->assign.target->type == NODE_VARIABLE_EXPR) {
        const char* name = node->assign.target->variable.name;
        int length = node->assign.target->variable.name_length;

        int index = compiler_resolve_local(compiler, name, length);
        if (index < 0) {
            index = compiler_add_local(compiler, name, length);
            if (index < 0) {
                compiler_error_at_line(compiler, node->line, "Failed to add variable");
                return COMPILE_ERROR;
            }
        }

        if (index <= 255) {
            emit_op(compiler, bytecode, OP_STORE_LOCAL_8);
            emit_u8(compiler, bytecode, (uint8_t)index);
        } else {
            emit_op(compiler, bytecode, OP_STORE_LOCAL_16);
            emit_u16(compiler, bytecode, (uint16_t)index);
        }
    } else if (node->assign.target->type == NODE_INDEX_EXPR) {
        result = compile_expression(compiler, node->assign.target->index.array, bytecode);
        if (result != COMPILE_SUCCESS) return result;

        result = compile_expression(compiler, node->assign.target->index.index, bytecode);
        if (result != COMPILE_SUCCESS) return result;

        emit_op(compiler, bytecode, OP_ARRAY_SET);
    } else {
        compiler_error_at_line(compiler, node->line, "Invalid assignment target");
        return COMPILE_ERROR;
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_call(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (node->call.callee->type == NODE_VARIABLE_EXPR) {
        const char* name = node->call.callee->variable.name;

        NativeInfo* native_info = native_get_info(name);
        if (native_info != NULL) {
            if (native_info->arity != -1 && (int)node->call.arg_count != native_info->arity) {
                char buffer[256];
                snprintf(buffer, sizeof(buffer), "%s() expects %d arguments, got %zu",
                         name, native_info->arity, node->call.arg_count);
                compiler_error_at_line(compiler, node->line, buffer);
                return COMPILE_ERROR;
            }

            if (strcmp(name, "print") == 0) {
                if (node->call.arg_count < 1) {
                    compiler_error_at_line(compiler, node->line,
                                           "print() expects at least 1 argument");
                    return COMPILE_ERROR;
                }

                for (size_t i = 0; i < node->call.arg_count; i++) {
                    CompileResult result = compile_expression(compiler,
                                                              node->call.arguments[i],
                                                              bytecode);
                    if (result != COMPILE_SUCCESS) return result;

                    emit_op(compiler, bytecode, OP_PRINT);
                }
                return COMPILE_SUCCESS;
            }

            for (size_t i = 0; i < node->call.arg_count; i++) {
                CompileResult result = compile_expression(compiler,
                                                          node->call.arguments[i],
                                                          bytecode);
                if (result != COMPILE_SUCCESS) return result;
            }

            int global_index = native_get_global_index(name);
            if (global_index < 0) {
                char buffer[256];
                snprintf(buffer, sizeof(buffer), "Native function '%s' not registered", name);
                compiler_error_at_line(compiler, node->line, buffer);
                return COMPILE_ERROR;
            }

            emit_op(compiler, bytecode, OP_LOAD_GLOBAL);
            emit_u16(compiler, bytecode, (uint16_t)global_index);

            if (node->call.arg_count <= 255) {
                emit_op(compiler, bytecode, OP_CALL_8);
                emit_u8(compiler, bytecode, (uint8_t)node->call.arg_count);
            } else {
                emit_op(compiler, bytecode, OP_CALL_16);
                emit_u16(compiler, bytecode, (uint16_t)node->call.arg_count);
            }

            return COMPILE_SUCCESS;
        }
    }

    if (node->call.callee->type == NODE_MEMBER_EXPR) {
        const char* method_name = node->call.callee->member.name;

        if (strcmp(method_name, "push") == 0) {
            CompileResult result = compile_expression(compiler,
                                                      node->call.callee->member.object,
                                                      bytecode);
            if (result != COMPILE_SUCCESS) return result;

            for (size_t i = 0; i < node->call.arg_count; i++) {
                result = compile_expression(compiler, node->call.arguments[i], bytecode);
                if (result != COMPILE_SUCCESS) return result;
            }

            int global_index = native_get_global_index("push");
            if (global_index < 0) {
                compiler_error_at_line(compiler, node->line,
                                       "push function not found");
                return COMPILE_ERROR;
            }

            emit_op(compiler, bytecode, OP_LOAD_GLOBAL);
            emit_u16(compiler, bytecode, (uint16_t)global_index);

            if (node->call.arg_count + 1 <= 255) {
                emit_op(compiler, bytecode, OP_CALL_8);
                emit_u8(compiler, bytecode, (uint8_t)(node->call.arg_count + 1));
            } else {
                emit_op(compiler, bytecode, OP_CALL_16);
                emit_u16(compiler, bytecode, (uint16_t)(node->call.arg_count + 1));
            }

            return COMPILE_SUCCESS;
        }
    }

    CompileResult result = compile_expression(compiler, node->call.callee, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    for (size_t i = 0; i < node->call.arg_count; i++) {
        result = compile_expression(compiler, node->call.arguments[i], bytecode);
        if (result != COMPILE_SUCCESS) return result;
    }

    if (node->call.arg_count <= 255) {
        emit_op(compiler, bytecode, OP_CALL_8);
        emit_u8(compiler, bytecode, (uint8_t)node->call.arg_count);
    } else {
        emit_op(compiler, bytecode, OP_CALL_16);
        emit_u16(compiler, bytecode, (uint16_t)node->call.arg_count);
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_logical(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->logical.left, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    size_t jump_addr;
    if (node->logical.operator == TOKEN_AND) {
        emit_jump(compiler, bytecode, OP_JUMP_IF_FALSE_8, &jump_addr);
    } else {
        emit_jump(compiler, bytecode, OP_JUMP_IF_TRUE_8, &jump_addr);
    }

    result = compile_expression(compiler, node->logical.right, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    patch_jump(compiler, bytecode, jump_addr);
    return COMPILE_SUCCESS;
}

CompileResult compile_array(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    for (size_t i = 0; i < node->array.element_count; i++) {
        CompileResult result = compile_expression(compiler, node->array.elements[i], bytecode);
        if (result != COMPILE_SUCCESS) return result;
    }

    emit_op(compiler, bytecode, OP_ARRAY_NEW);
    emit_u8(compiler, bytecode, (uint8_t)node->array.element_count);
    return COMPILE_SUCCESS;
}

CompileResult compile_index(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->index.array, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    result = compile_expression(compiler, node->index.index, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    emit_op(compiler, bytecode, OP_ARRAY_GET);
    return COMPILE_SUCCESS;
}

CompileResult compile_member(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->member.object, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    int const_idx = bc_add_constant_string(bytecode, node->member.name);
    emit_op(compiler, bytecode, OP_LOAD_CONST);
    emit_u32(compiler, bytecode, (uint32_t)const_idx);

    emit_op(compiler, bytecode, OP_ARRAY_GET);
    return COMPILE_SUCCESS;
}

CompileResult compile_if(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->if_stmt.condition, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    size_t then_jump;
    emit_jump(compiler, bytecode, OP_JUMP_IF_FALSE_8, &then_jump);

    result = compile_statement(compiler, node->if_stmt.then_branch, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    if (node->if_stmt.else_branch) {
        size_t else_jump;
        emit_jump(compiler, bytecode, OP_JUMP_8, &else_jump);

        patch_jump(compiler, bytecode, then_jump);

        result = compile_statement(compiler, node->if_stmt.else_branch, bytecode);
        if (result != COMPILE_SUCCESS) return result;

        patch_jump(compiler, bytecode, else_jump);
    } else {
        patch_jump(compiler, bytecode, then_jump);
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_for(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    compiler_begin_loop(compiler);
    int start_count = compiler->locals.count;
    compiler_begin_scope(compiler);
    int saved_break_count = compiler->loop.break_count;
    int saved_continue_count = compiler->loop.continue_count;

    if (node->for_stmt.initializer) {
        CompileResult result = compile_statement(compiler, node->for_stmt.initializer, bytecode);
        if (result != COMPILE_SUCCESS) return result;
    }

    size_t loop_start = bc_get_current_address(bytecode);

    size_t exit_jump;
    //int has_exit_jump = 0;

    if (node->for_stmt.condition) {
        CompileResult result = compile_expression(compiler, node->for_stmt.condition, bytecode);
        if (result != COMPILE_SUCCESS) return result;

        emit_jump(compiler, bytecode, OP_JUMP_IF_FALSE_8, &exit_jump);
        //has_exit_jump = 1;

        if (compiler->loop.break_count >= compiler->loop.break_capacity) {
            compiler->loop.break_capacity *= 2;
            compiler->loop.break_jumps = GROW_ARRAY(
                    size_t,
                    compiler->loop.break_jumps,
                    compiler->loop.break_count,
                    compiler->loop.break_capacity
            );
        }

        compiler->loop.break_jumps[compiler->loop.break_count++] = exit_jump;

    }

    CompileResult result = compile_statement(compiler, node->for_stmt.body, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    size_t continue_start = bc_get_current_address(bytecode);

    if (node->for_stmt.increment) {
        result = compile_expression(compiler, node->for_stmt.increment, bytecode);
        if (result != COMPILE_SUCCESS) return result;
        emit_op(compiler, bytecode, OP_POP);
    }

    emit_loop(compiler, bytecode, loop_start);

    for (int i = 0; i < compiler->loop.continue_count; i++) {
        compiler_patch_continues(compiler, bytecode, continue_start);
    }
    compiler->loop.continue_count = saved_continue_count;

    size_t current_end = bc_get_current_address(bytecode);
    for (int i = 0; i < compiler->loop.break_count; i++) {
        bc_patch_i16(bytecode, compiler->loop.break_jumps[i], (int16_t)(current_end - compiler->loop.break_jumps[i] - 2));
    }
    compiler->loop.break_count = saved_break_count;;

    compiler_end_scope(compiler, bytecode, start_count);
    compiler_end_loop(compiler, bytecode);

    // Clean up loop locals
    // while (compiler->locals.count > start_count) {
    //     compiler->locals.count--;
    //     if (compiler->locals.names[compiler->locals.count]) {
    //         FREE_ARRAY(char, compiler->locals.names[compiler->locals.count],
    //                    strlen(compiler->locals.names[compiler->locals.count]) + 1);
    //     }
    // }

    return COMPILE_SUCCESS;
}


CompileResult compile_function(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    compiler_begin_function(compiler, node->function_stmt.name);

    //int start_count = compiler->locals.count;
    compiler_begin_scope(compiler);

    if (node->function_stmt.name) {
        compiler_add_local(compiler, node->function_stmt.name, strlen(node->function_stmt.name));
    }

    for (size_t i = 0; i < node->function_stmt.param_count; i++) {
        compiler_add_local(compiler, node->function_stmt.parameters[i],
                           strlen(node->function_stmt.parameters[i]));
    }

    CompileResult result = compile_block(compiler, node->function_stmt.body, bytecode);
    if (result != COMPILE_SUCCESS) {
        compiler_end_function(compiler);
        return result;
    }

    if (compiler->function.return_count == 0) {
        emit_op(compiler, bytecode, OP_RETURN_NIL);
    }

    compiler_end_function(compiler);

    return COMPILE_SUCCESS;
}

CompileResult compile_return(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    if (compiler->function.function_depth == 0) {
        compiler_error_at_line(compiler, node->line, "'return' outside function");
        return COMPILE_ERROR;
    }

    compiler->function.return_count++;

    if (node->return_stmt.value) {
        CompileResult result = compile_expression(compiler, node->return_stmt.value, bytecode);
        if (result != COMPILE_SUCCESS) return result;
        emit_op(compiler, bytecode, OP_RETURN);
    } else {
        emit_op(compiler, bytecode, OP_RETURN_NIL);
    }

    return COMPILE_SUCCESS;
}

CompileResult compile_break(Compiler* compiler, Bytecode* bytecode) {
    if (compiler->loop.loop_depth == 0) {
        compiler_error(compiler, "'break' outside loop");
        return COMPILE_ERROR;
    }

    size_t jump_address;
    emit_jump(compiler, bytecode, OP_JUMP_8, &jump_address);

    if (compiler->loop.break_count >= compiler->loop.break_capacity) {
        compiler->loop.break_capacity *= 2;
        compiler->loop.break_jumps = GROW_ARRAY(size_t, compiler->loop.break_jumps,
                                                compiler->loop.break_count,
                                                compiler->loop.break_capacity);
    }

    compiler->loop.break_jumps[compiler->loop.break_count++] = jump_address;
    return COMPILE_SUCCESS;
}

CompileResult compile_continue(Compiler* compiler, Bytecode* bytecode) {
    if (compiler->loop.loop_depth == 0) {
        compiler_error(compiler, "'continue' outside loop");
        return COMPILE_ERROR;
    }

    size_t jump_address;
    emit_jump(compiler, bytecode, OP_JUMP_8, &jump_address);

    if (compiler->loop.continue_count >= compiler->loop.continue_capacity) {
        compiler->loop.continue_capacity *= 2;
        compiler->loop.continue_jumps = GROW_ARRAY(size_t, compiler->loop.continue_jumps,
                                                   compiler->loop.continue_count,
                                                   compiler->loop.continue_capacity);
    }

    compiler->loop.continue_jumps[compiler->loop.continue_count++] = jump_address;
    return COMPILE_SUCCESS;
}

int compiler_resolve_local(Compiler* compiler, const char* name, int length) {
    for (int i = compiler->locals.count - 1; i >= 0; i--) {
        if (compiler->locals.depths[i] != -1 &&
            compiler->locals.names[i] != NULL &&
            strncmp(compiler->locals.names[i], name, (size_t)length) == 0 &&
            (int)strlen(compiler->locals.names[i]) == length) {
            return i;
        }
    }
    return -1;
}

int compiler_add_local(Compiler* compiler, const char* name, int length) {
    if (compiler->locals.count >= compiler->locals.capacity) {
        int new_capacity = compiler->locals.capacity * 2;
        compiler->locals.names = GROW_ARRAY(char*, compiler->locals.names,
                                            compiler->locals.capacity, new_capacity);
        compiler->locals.depths = GROW_ARRAY(int, compiler->locals.depths,
                                             compiler->locals.capacity, new_capacity);
        compiler->locals.capacity = new_capacity;
    }

    compiler->locals.names[compiler->locals.count] = ALLOCATE(char, length + 1);
    memcpy(compiler->locals.names[compiler->locals.count], name, length);
    compiler->locals.names[compiler->locals.count][length] = '\0';
    compiler->locals.depths[compiler->locals.count] = compiler->scope_depth;

    return compiler->locals.count++;
}

void compiler_begin_scope(Compiler* compiler) {
    compiler->scope_depth++;
}

void compiler_end_scope(Compiler* compiler, Bytecode* bytecode, int start_count) {
    compiler->scope_depth--;

    // Pop locals that were added in this scope
    while (compiler->locals.count > start_count) {
        emit_op(compiler, bytecode, OP_POP);
        compiler->locals.count--;

        compiler->locals.depths[compiler->locals.count] = -1;
    }
}

void compiler_begin_loop(Compiler* compiler) {
    compiler->loop.loop_depth++;
    init_loop_jumps(compiler);
}

void compiler_end_loop(Compiler* compiler, Bytecode* bytecode) {
    (void)bytecode;
    compiler->loop.loop_depth--;
}

void compiler_patch_breaks(Compiler* compiler, Bytecode* bytecode, size_t target) {
    for (int i = 0; i < compiler->loop.break_count; i++) {
        size_t jump_address = compiler->loop.break_jumps[i];
        int16_t offset = (int16_t)(target - jump_address - 1);
        bc_patch_i16(bytecode, jump_address, offset);
    }
}

void compiler_patch_continues(Compiler* compiler, Bytecode* bytecode, size_t target) {
    for (int i = 0; i < compiler->loop.continue_count; i++) {
        size_t jump_address = compiler->loop.continue_jumps[i];
        int16_t offset = (int16_t)(target - jump_address - 1);
        bc_patch_i16(bytecode, jump_address, offset);
    }
}

void compiler_begin_function(Compiler* compiler, const char* name) {
    compiler->function.function_depth++;
    compiler->function.return_count = 0;

    if (name) {
        compiler->function.current_function = ALLOCATE(char, strlen(name) + 1);
        strcpy(compiler->function.current_function, name);
    } else {
        compiler->function.current_function = NULL;
    }
}

void compiler_end_function(Compiler* compiler) {
    compiler->function.function_depth--;
    if (compiler->function.current_function) {
        FREE_ARRAY(char, compiler->function.current_function,
                   strlen(compiler->function.current_function) + 1);
        compiler->function.current_function = NULL;
    }
}

void emit_byte(Compiler* compiler, Bytecode* bytecode, uint8_t byte) {
    bc_write_byte(bytecode, byte, compiler->current_line);
}

void emit_op(Compiler* compiler, Bytecode* bytecode, OpCode op) {
    bc_write_op(bytecode, op, compiler->current_line);
}

void emit_i8(Compiler* compiler, Bytecode* bytecode, int8_t value) {
    bc_write_i8(bytecode, value, compiler->current_line);
}

void emit_i16(Compiler* compiler, Bytecode* bytecode, int16_t value) {
    bc_write_i16(bytecode, value, compiler->current_line);
}

void emit_i32(Compiler* compiler, Bytecode* bytecode, int32_t value) {
    bc_write_i32(bytecode, value, compiler->current_line);
}

void emit_u8(Compiler* compiler, Bytecode* bytecode, uint8_t value) {
    bc_write_u8(bytecode, value, compiler->current_line);
}

void emit_u16(Compiler* compiler, Bytecode* bytecode, uint16_t value) {
    bc_write_u16(bytecode, value, compiler->current_line);
}

void emit_u32(Compiler* compiler, Bytecode* bytecode, uint32_t value) {
    bc_write_u32(bytecode, value, compiler->current_line);
}

void emit_const(Compiler* compiler, Bytecode* bytecode, int index) {
    emit_op(compiler, bytecode, OP_LOAD_CONST);
    emit_u32(compiler, bytecode, (uint32_t)index);
}

void emit_jump(Compiler* compiler, Bytecode* bytecode, OpCode op, size_t* jump_address) {
    emit_op(compiler, bytecode, op);
    *jump_address = bc_get_current_address(bytecode);
    emit_i16(compiler, bytecode, 0);
}

void emit_loop(Compiler* compiler, Bytecode* bytecode, size_t loop_start) {
    emit_op(compiler, bytecode, OP_JUMP_8);
    int16_t offset = (int16_t)(loop_start - bc_get_current_address(bytecode) - 1);
    emit_i16(compiler, bytecode, offset);
}

void patch_jump(Compiler* compiler, Bytecode* bytecode, size_t jump_address) {
    (void)compiler;
    size_t current = bc_get_current_address(bytecode);
    int16_t offset = (int16_t)(current - jump_address - 2);
    bc_patch_i16(bytecode, jump_address, offset);
}

void compiler_error(Compiler* compiler, const char* message) {
    compiler->error_count++;
    compiler->error_message = message;

    fprintf(stderr, "[%s] Compile error: %s\n",
            compiler->source_file ? compiler->source_file : "<source>",
            message);
}

void compiler_error_at_line(Compiler* compiler, int line, const char* message) {
    compiler->error_count++;
    compiler->error_message = message;

    fprintf(stderr, "[%s:%d] Compile error: %s\n",
            compiler->source_file ? compiler->source_file : "<source>",
            line, message);
}

int get_operator_precedence(TokenType type) {
    switch (type) {
        case TOKEN_OR: return 1;
        case TOKEN_AND: return 2;
        case TOKEN_EQUAL_EQUAL:
        case TOKEN_BANG_EQUAL: return 3;
        case TOKEN_GREATER:
        case TOKEN_GREATER_EQUAL:
        case TOKEN_LESS:
        case TOKEN_LESS_EQUAL: return 4;
        case TOKEN_BIT_OR: return 5;
        case TOKEN_BIT_XOR: return 6;
        case TOKEN_BIT_AND: return 7;
        case TOKEN_BIT_LEFT_SHIFT:
        case TOKEN_BIT_RIGHT_SHIFT: return 8;
        case TOKEN_PLUS:
        case TOKEN_MINUS: return 9;
        case TOKEN_STAR:
        case TOKEN_SLASH:
        case TOKEN_PERCENT: return 10;
        default: return 0;
    }
}

OpCode get_binary_opcode(TokenType type) {
    switch (type) {
        case TOKEN_PLUS: return OP_ADD;
        case TOKEN_MINUS: return OP_SUB;
        case TOKEN_STAR: return OP_MUL;
        case TOKEN_SLASH: return OP_DIV;
        case TOKEN_PERCENT: return OP_MOD;
        case TOKEN_EQUAL_EQUAL: return OP_EQ;
        case TOKEN_BANG_EQUAL: return OP_NEQ;
        case TOKEN_GREATER: return OP_GT;
        case TOKEN_GREATER_EQUAL: return OP_GE;
        case TOKEN_LESS: return OP_LT;
        case TOKEN_LESS_EQUAL: return OP_LE;
        case TOKEN_AND: return OP_AND;
        case TOKEN_OR: return OP_OR;
        default: return 0;
    }
}

OpCode get_unary_opcode(TokenType type) {
    switch (type) {
        case TOKEN_BANG: return OP_NOT;
        case TOKEN_MINUS: return OP_NEG;
        case TOKEN_BIT_NOT: return OP_NOT;
        default: return 0;
    }
}

OpCode get_postfix_opcode(TokenType type) {
    (void)type;
    return 0;
}

int should_emit_short(int value) {
    return value >= -128 && value <= 127;
}

CompileResult compile_ternary(Compiler* compiler, const ASTNode* node, Bytecode* bytecode) {
    CompileResult result = compile_expression(compiler, node->ternary.condition, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    size_t false_jump;
    emit_jump(compiler, bytecode, OP_JUMP_IF_FALSE_8, &false_jump);

    result = compile_expression(compiler, node->ternary.then_expr, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    size_t end_jump;
    emit_jump(compiler, bytecode, OP_JUMP_8, &end_jump);

    patch_jump(compiler, bytecode, false_jump);

    result = compile_expression(compiler, node->ternary.else_expr, bytecode);
    if (result != COMPILE_SUCCESS) return result;

    patch_jump(compiler, bytecode, end_jump);

    return COMPILE_SUCCESS;
}
