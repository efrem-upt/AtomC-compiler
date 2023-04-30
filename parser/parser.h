#pragma once

#include "../alex/lexer.h"
#include <stdbool.h>

void parse(Token *tokens);
void tkerr(const char* fmt, ...);
bool consume(int code);
bool exprPrimary();
bool exprPostfix();
bool exprUnary();
bool exprCast();
bool exprMul();
bool exprAdd();
bool exprRel();
bool exprEq();
bool exprAnd();
bool exprOr();
bool exprAssign();
bool expr();
bool stmCompound(bool);
bool stm();
bool fnParam();
bool fnDef();
bool arrayDecl();
bool typeBase();
bool varDef();
bool structDef();
bool unit();
char* tkCode();