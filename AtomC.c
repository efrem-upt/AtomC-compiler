#include <stdio.h>
#include "utils/utils.h"
#include "alex/lexer.h"

int main(int argc, char* argv[]) {
    char *inbuf = loadFile("tests/testlex.c");
    puts(inbuf);
    Token* tokens = tokenize(inbuf);
    showTokens(tokens);
    return 0;
}