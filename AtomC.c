#include <stdio.h>
#include <stdlib.h>
#include "utils/utils.h"
#include "alex/lexer.h"
#include "parser/parser.h"
#include "parser/ad.h"
#include "parser/vm.h"

int main(int argc, char* argv[]) {
    char *inbuf = loadFile("tests/testgc.c");
    Token* tokens = tokenize(inbuf);
    showTokens(tokens);
    pushDomain();
    vmInit();
    parse(tokens);
    Symbol *symMain=findSymbolInDomain(symTable,"main");
    if(!symMain)err("missing main function");
    Instr *entryCode=NULL;
    addInstr(&entryCode,OP_CALL)->arg.instr=symMain->fn.instr;
    addInstr(&entryCode,OP_HALT);
    run(entryCode);
    dropDomain();
    freeDinamicallyAllocatedElements();
    return 0;
}