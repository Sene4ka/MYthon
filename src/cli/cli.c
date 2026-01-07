#include "cli.h"
#include "core/compiler.h"
#include "core/lexer.h"
#include "core/parser.h"
#include "runtime/vm.h"
#include "bytecode.h"
#include "utils/utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "native.h"

CLIArgs parse_args(int argc, char** argv) {
    CLIArgs args;
    args.mode = MODE_RUN;
    args.input_file = NULL;
    args.output_file = NULL;

    args.debug_parse   = 0;
    args.debug_compile = 0;
    args.debug_vm      = 0;
    args.debug_gc      = 0;
    args.debug_memory  = 0;

    args.verbose    = 0;
    args.disassemble = 0;
    args.jit        = 0;

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
                args.mode = MODE_HELP;
                return args;
            } else if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--version")) {
                args.mode = MODE_VERSION;
                return args;
            } else if (!strcmp(argv[i], "-c") || !strcmp(argv[i], "--compile")) {
                args.mode = MODE_COMPILE;
                if (i + 1 < argc) {
                    args.input_file = strdup(argv[++i]);
                }
            } else if (!strcmp(argv[i], "-r") || !strcmp(argv[i], "--exec")) {
                args.mode = MODE_EXEC;
                if (i + 1 < argc) {
                    args.input_file = strdup(argv[++i]);
                }
            } else if (!strcmp(argv[i], "-D") || !strcmp(argv[i], "--disassemble")) {
                args.mode = MODE_DISASM;
                if (i + 1 < argc) {
                    args.input_file = strdup(argv[++i]);
                }
            } else if (!strcmp(argv[i], "-o") || !strcmp(argv[i], "--output")) {
                if (i + 1 < argc) {
                    args.output_file = strdup(argv[++i]);
                }
            }
            else if (!strcmp(argv[i], "-d") || !strcmp(argv[i], "--debug")) {
                args.debug_parse   = 1;
                args.debug_compile = 1;
                args.debug_vm      = 1;
                args.debug_gc      = 1;
                args.debug_memory  = 1;
            }
            else if (!strcmp(argv[i], "--debug-parse")) {
                args.debug_parse = 1;
            } else if (!strcmp(argv[i], "--debug-compile")) {
                args.debug_compile = 1;
            } else if (!strcmp(argv[i], "--debug-vm")) {
                args.debug_vm = 1;
            } else if (!strcmp(argv[i], "--debug-gc")) {
                args.debug_gc = 1;
            } else if (!strcmp(argv[i], "--debug-memory")) {
                args.debug_memory = 1;
            }
            else if (!strcmp(argv[i], "-V") || !strcmp(argv[i], "--verbose")) {
                args.verbose = 1;
            } else if (!strcmp(argv[i], "--jit")) {
                args.jit = 1;
            } else {
                fprintf(stderr, "Unknown option: %s\n", argv[i]);
                exit(1);
            }
        } else {
            if (!args.input_file) {
                args.input_file = strdup(argv[i]);
            }
        }
    }

    return args;
}



void print_usage(void) {
    printf("Usage: mython [options] [file]\n");
    printf("\nOptions:\n");
    printf("  -h, --help             Show this help\n");
    printf("  -v, --version          Show version\n");
    printf("  -c, --compile          Compile to bytecode\n");
    printf("  -o <file>              Specify output file\n");
    printf("  -d, --debug            Enable all debug modes\n");
    printf("      --debug-parse      Debug parser / AST\n");
    printf("      --debug-compile    Debug compiler / bytecode gen\n");
    printf("      --debug-vm         Debug VM execution\n");
    printf("      --debug-gc         Debug GC\n");
    printf("      --debug-memory     Debug memory allocator\n");
    printf("  -V, --verbose          Verbose output\n");
    printf("  -D, --disassemble      Disassemble bytecode\n");
    printf("      --jit              Enable JIT\n");
}

void print_version(void) {
    printf("Mython 1.0.0\n");
}

static Bytecode* compile_source(const char* source,
                                const char* filename,
                                int debug_parse,
                                int debug_compile) {
    Lexer lexer;
    lexer_init(&lexer, (const unsigned char*)source);

    Parser parser;
    parser_init(&parser, &lexer, filename);

    ASTNode* ast = parser_parse(&parser);

    if (parser_has_errors(&parser)) {
        fprintf(stderr, "Parse errors: %d errors\n", parser_error_count(&parser));
        ast_free(ast);
        return NULL;
    }

    if (debug_parse) {
        ast_print(ast, 0);
    }

    Bytecode* bc = bytecode_new();
    Compiler compiler;
    compiler_init(&compiler, filename);

    compiler.debug = debug_compile;

    CompileResult result = compiler_compile(&compiler, ast, bc);

    if (result != COMPILE_SUCCESS) {
        fprintf(stderr, "Compilation failed\n");
        bytecode_free(bc);
        ast_free(ast);
        return NULL;
    }

    ast_free(ast);
    return bc;
}


int run_file(CLIArgs args) {
    size_t length;
    char* source = read_entire_file(args.input_file, &length);
    if (!source) {
        fprintf(stderr, "Failed to read file: %s\n", args.input_file);
        return 1;
    }

    if (args.verbose) {
        printf("Compiling %s...\n", args.input_file);
    }

    Bytecode* bc = compile_source(
        source,
        args.input_file,
        args.debug_parse,
        args.debug_compile
    );

    if (!bc) {
        free(source);
        return 1;
    }

    if (args.disassemble) {
        bc_disassemble(bc, args.input_file);
    }

    VM* vm = vm_new();
    native_register_all(vm);

    if (args.debug_vm) {
        vm_set_debug(vm, 1);
        vm->debug_level = DEBUG_GLOBAL;
    }

    vm->debug_gc = args.debug_gc;
    vm->next_gc = 1024;

    //vm->jit = args.jit

    InterpretResult result = vm_run(vm, bc);

    if (result == INTERPRET_COMPILE_ERROR) {
        fprintf(stderr, "Compile error\n");
    } else if (result == INTERPRET_RUNTIME_ERROR) {
        fprintf(stderr, "Runtime error\n");
    }

    vm_free(vm);
    bytecode_free(bc);
    free(source);

    return result == INTERPRET_OK ? 0 : 1;
}

int compile_file(CLIArgs args) {
    size_t length;
    char* source = read_entire_file(args.input_file, &length);
    if (!source) {
        fprintf(stderr, "Failed to read file: %s\n", args.input_file);
        return 1;
    }

    Bytecode* bc = compile_source(
        source,
        args.input_file,
        args.debug_parse,
        args.debug_compile
    );

    if (!bc) {
        free(source);
        return 1;
    }

    if (args.disassemble) {
        bc_disassemble(bc, args.input_file);
    }

    const char* output = args.output_file ? args.output_file : "output.cmth";
    int success = bytecode_save(bc, output);

    if (success) {
        printf("Compiled to %s\n", output);
    } else {
        fprintf(stderr, "Failed to save file: %s\n", output);
    }

    bytecode_free(bc);
    free(source);

    return success ? 0 : 1;
}

int disassemble_file(CLIArgs args) {
    Bytecode* bc = bytecode_load(args.input_file);
    if (!bc) {
        fprintf(stderr, "Failed to load file: %s\n", args.input_file);
        return 1;
    }

    bc_disassemble(bc, args.input_file);
    bytecode_free(bc);
    return 0;
}

int exec_bytecode_file(CLIArgs args) {
    Bytecode* bc = bytecode_load(args.input_file);
    if (!bc) {
        fprintf(stderr, "Failed to load bytecode: %s\n", args.input_file);
        return 1;
    }

    VM* vm = vm_new();
    native_register_all(vm);

    if (args.debug_vm) {
        vm_set_debug(vm, 1);
        vm->debug_level = DEBUG_GLOBAL;
    }

    InterpretResult result = vm_run(vm, bc);

    if (result == INTERPRET_RUNTIME_ERROR) {
        fprintf(stderr, "Runtime error\n");
    }

    vm_free(vm);
    bytecode_free(bc);
    return result == INTERPRET_OK ? 0 : 1;
}