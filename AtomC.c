#include <stdio.h>
#include <stdlib.h>
#include "utils/utils.h"
#include "alex/lexer.h"
#include "parser/parser.h"

int main(int argc, char* argv[]) {
    char *inbuf = loadFile("tests/testparser.c");
    Token* tokens = tokenize(inbuf);
    showTokens(tokens);
    parse(tokens);
    freeDinamicallyAllocatedElements();
    return 0;
}