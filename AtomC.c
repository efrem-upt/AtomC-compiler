#include <stdio.h>
#include <stdlib.h>
#include "utils/utils.h"
#include "alex/lexer.h"
#include "parser/parser.h"
#include "parser/ad.h"

int main(int argc, char* argv[]) {
    char *inbuf = loadFile("tests/testad.c");
    Token* tokens = tokenize(inbuf);
    showTokens(tokens);
    pushDomain();
    parse(tokens);
    showDomain(symTable,"global"); // afisare domeniu global
    dropDomain();
    freeDinamicallyAllocatedElements();
    return 0;
}