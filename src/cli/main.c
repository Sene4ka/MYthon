#include "cli/cli.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <windows.h>
#endif

int main(int argc, char *argv[]) {
#ifdef _WIN32
    SetConsoleOutputCP(CP_UTF8);
#endif

    printf("DEBUG: enter main\n");
    CLIArgs args = parse_args(argc, argv);
    int result = 0;
    printf("DEBUG: exit parse args\n");

    switch (args.mode) {
        case MODE_RUN:
            printf("DEBUG: enter run\n");
            result = run_file(args);
            printf("DEBUG: exit run\n");
            break;
        case MODE_COMPILE:
            result = compile_file(args);
            break;
        case MODE_EXEC:
            result = exec_bytecode_file(args);
            break;
        case MODE_DISASM:
            result = disassemble_file(args);
            break;
        case MODE_REPL:
            result = start_repl(args);
            break;
        case MODE_HELP:
            print_usage();
            break;
        case MODE_VERSION:
            print_version();
            break;
        default:
            fprintf(stderr, "Unknown mode\n");
            result = 1;
    }

    printf("Finished with exit code %d\n", result);
    fflush(stdout);

    free(args.input_file);
    free(args.output_file);
    return result;
}
