// cli.h - Add new mode and function declaration
#ifndef CLI_H
#define CLI_H

typedef enum {
    MODE_RUN,
    MODE_EXEC,
    MODE_COMPILE,
    MODE_REPL,
    MODE_HELP,
    MODE_VERSION,
    MODE_DISASM
} Mode;

typedef struct {
    Mode mode;
    char* input_file;
    char* output_file;
    int optimize;
    int debug;
    int verbose;
    int disassemble;
} CLIArgs;

CLIArgs parse_args(int argc, char** argv);
void print_usage(void);
void print_version(void);
int run_file(CLIArgs args);
int compile_file(CLIArgs args);
int start_repl(CLIArgs args);
int disassemble_file(CLIArgs args);
int exec_bytecode_file(CLIArgs args);

#endif