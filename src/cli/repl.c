#include "repl.h"
#include "core/compiler.h"
#include "core/lexer.h"
#include "core/parser.h"
#include "utils/memory.h"
#include "utils/utils.h"
#include "runtime/native.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

REPL* repl_new(void) {
    REPL* repl = ALLOCATE(REPL, 1);
    repl->vm = vm_new();
    if (!repl->vm) {
        free_ptr(repl);
        return NULL;
    }

    native_register_all(repl->vm);

    repl->running = 1;
    repl->line_number = 1;
    repl->history_count = 0;
    repl->show_ast = 0;
    repl->show_bytecode = 0;

    for (int i = 0; i < 100; i++) {
        repl->history[i] = NULL;
    }

    return repl;
}

void repl_free(REPL* repl) {
    if (!repl) return;

    if (repl->vm) {
        vm_free(repl->vm);
    }

    for (int i = 0; i < repl->history_count; i++) {
        free_ptr(repl->history[i]);
    }

    free_ptr(repl);
}

static Bytecode* compile_source(VM* vm, const char* source, const char* filename, int show_ast, int show_bytecode) {
    (void)vm;
    Lexer lexer;
    lexer_init(&lexer, (const unsigned char*)source);

    Parser parser;
    parser_init(&parser, &lexer, filename);

    ASTNode* ast = parser_parse(&parser);
    if (parser_has_errors(&parser)) {
        fprintf(stderr, "Ошибки при парсинге\n");
        ast_free(ast);
        return NULL;
    }

    if (show_ast) {
        ast_print(ast, 0);
    }

    Bytecode* bc = bytecode_new();
    Compiler compiler;
    compiler_init(&compiler, filename);

    CompileResult result = compiler_compile(&compiler, ast, bc);
    if (result != COMPILE_SUCCESS) {
        fprintf(stderr, "Ошибки при компиляции\n");
        bytecode_free(bc);
        ast_free(ast);
        return NULL;
    }

    if (show_bytecode) {
        bc_disassemble(bc);
    }

    ast_free(ast);
    return bc;
}

void repl_eval(REPL* repl, const char* source) {
    Bytecode* bc = compile_source(repl->vm, source, "<repl>", repl->show_ast, repl->show_bytecode);
    if (!bc) {
        return;
    }

    InterpretResult result = vm_run(repl->vm, bc);
    if (result == INTERPRET_OK && repl->vm->sp > 0) {
        Value top = vm_peek(repl->vm, 0);
        repl_print_result(top);
        vm_pop(repl->vm);
    }

    bytecode_free(bc);
}

void repl_print_result(Value value) {
    if (IS_NIL(value)) {
        printf("nil\n");
    } else if (IS_BOOL(value)) {
        printf("%s\n", AS_BOOL(value) ? "true" : "false");
    } else if (IS_INT(value)) {
        printf("%lld\n", (long long)AS_INT(value));
    } else if (IS_FLOAT(value)) {
        printf("%g\n", AS_FLOAT(value));
    } else if (IS_STRING(value)) {
        StringObject* str = AS_STRING(value);
        fwrite(str->chars, 1, str->length, stdout);
        printf("\n");
    } else {
        const char* type = vm_value_type_name(value);
        printf("[%s]\n", type);
    }
}

static void add_to_history(REPL* repl, const char* line) {
    if (repl->history_count >= 100) {
        free_ptr(repl->history[0]);
        for (int i = 1; i < 100; i++) {
            repl->history[i-1] = repl->history[i];
        }
        repl->history[99] = strdup(line);
    } else {
        repl->history[repl->history_count++] = strdup(line);
    }
}

static void print_history(REPL* repl) {
    printf("История команд:\n");
    for (int i = 0; i < repl->history_count; i++) {
        printf("%3d: %s\n", i + 1, repl->history[i]);
    }
}

static void handle_command(REPL* repl, const char* line) {
    if (strcmp(line, ".exit") == 0 || strcmp(line, ".quit") == 0) {
        repl->running = 0;
    } else if (strcmp(line, ".clear") == 0) {
        printf("\033[H\033[J");
    } else if (strcmp(line, ".ast") == 0) {
        repl->show_ast = !repl->show_ast;
        printf("Показ AST: %s\n", repl->show_ast ? "вкл" : "выкл");
    } else if (strcmp(line, ".bytecode") == 0) {
        repl->show_bytecode = !repl->show_bytecode;
        printf("Показ байт-кода: %s\n", repl->show_bytecode ? "вкл" : "выкл");
    } else if (strcmp(line, ".history") == 0) {
        print_history(repl);
    } else if (strcmp(line, ".help") == 0) {
        printf("Команды REPL:\n");
        printf("  .exit/.quit   Выход\n");
        printf("  .clear        Очистить экран\n");
        printf("  .ast          Включить/выключить показ AST\n");
        printf("  .bytecode     Включить/выключить показ байт-кода\n");
        printf("  .history      Показать историю команд\n");
        printf("  .help         Эта справка\n");
    } else {
        printf("Неизвестная команда: %s\n", line);
    }
}

static char* read_line(FILE* stream) {
    char buffer[1024];

    if (fgets(buffer, sizeof(buffer), stream) == NULL) {
        return NULL;
    }

    size_t length = strlen(buffer);
    if (length > 0 && buffer[length-1] == '\n') {
        buffer[length-1] = '\0';
        length--;
    }

    char* line = ALLOCATE(char, length + 1);
    if (!line) {
        return NULL;
    }

    strcpy(line, buffer);
    return line;
}

int repl_run(REPL* repl, CLIArgs args) {
    (void)args;

    printf("Mython REPL (версия 0.1.0)\n");
    printf("Введите .help для списка команд\n");
    printf("Введите .exit или .quit для выхода\n\n");

    while (repl->running) {
        printf(">>> ");
        fflush(stdout);

        char* line = read_line(stdin);
        if (!line) {
            break;
        }

        if (strlen(line) == 0) {
            free_ptr(line);
            continue;
        }

        if (line[0] == '.') {
            handle_command(repl, line);
        } else {
            add_to_history(repl, line);
            repl_eval(repl, line);
            repl->line_number++;
        }

        free_ptr(line);
    }

    printf("\nВыход из REPL\n");
    return 0;
}