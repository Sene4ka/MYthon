#include "dfa_lexer.h"
#include <stdio.h>

void print_dfa_tables(void) {
    dfa_lexer_build_tables();

    printf("/* Автоматически сгенерированные таблицы DFA */\n");
    printf("static const DFAAction transition_table[256][64] = {\n");

    for (int c = 0; c < 256; c++) {
        printf("    { /* char %d (%c) */\n", c, c >= 32 && c < 127 ? c : ' ');
        for (int s = 0; s < 64; s++) {
            DFAAction action = transition_table[c][s];
            printf("        {%d, %d, %d},\n",
                   action.type, action.token_type, action.next_state);
        }
        printf("    },\n");
    }

    printf("};\n");
}

int main(int argc, char** argv) {
    print_dfa_tables();
    return 0;
}