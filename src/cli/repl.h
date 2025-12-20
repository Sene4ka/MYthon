#ifndef MYTHON_REPL_H
#define MYTHON_REPL_H

#include "cli.h"
#include "core/vm.h"

typedef struct {
    VM* vm;
    int running;
    int line_number;
    char* history[100];
    int history_count;
    int show_ast;
    int show_bytecode;
} REPL;

REPL* repl_new(void);
void repl_free(REPL* repl);
int repl_run(REPL* repl, CLIArgs args);
void repl_eval(REPL* repl, const char* source);
void repl_print_result(Value result);

#endif