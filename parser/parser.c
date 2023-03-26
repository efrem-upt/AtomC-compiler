#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include "../utils/utils.h"

#include "parser.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

void tkerr(const char *fmt,...){
	fprintf(stderr,"error in line %d: ",iTk->line);
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

bool consume(int code){
	printf("Consuming (%s) line %d ", tkCode(code), iTk->line);
	if(iTk->code==code){
		printf(" => consumed\n");
		consumedTk=iTk;
		iTk=iTk->next;
		return true;
		}
	printf(" => found %s\n", tkCode(iTk->code));
	return false;
	}

// exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR

bool exprPrimary() {
	printf("#exprPrimary %s\n",tkCode(iTk->code));
	Token* start = iTk;
	if(consume(ID)){
		if(consume(LPAR)){
			if(consume(RPAR)){
				return true;
				}
			if(expr()){
				while(consume(COMMA)){
					if(expr()){
						continue;
						} else tkerr("Missing function argument after ,");
					iTk = start;
					return false;
					}
				if(consume(RPAR)){
					return true;
					} else tkerr("Missing ) to end function call");
				}
			}
		return true;
		}
	iTk = start;
	if(consume(INT) || consume(DOUBLE) || consume(CHAR) || consume(STRING)){
		return true;
		}
	if(consume(LPAR)){
		if(expr()){
			if(consume(RPAR)){
				return true;
				} else tkerr("Missing ) after expression");
			}
		}
	iTk = start;
	return false;
}

// exprPostfix: exprPostfix LBRACKET expr RBRACKET | exprPostfix DOT ID | exprPrimary
// transformed into exprPostfix: exprPrimary exprPostfixPrim, exprPostfixPrim: (LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim)?

bool exprPostfixPrim() {
	printf("#exprPostfixPrim %s\n",tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LBRACKET)){
		if(expr()){
			if(consume(RBRACKET)){
				if(exprPostfixPrim()){
					return true;
					}
				} else tkerr("Missing ] after expression");
			}
		}
	iTk = start;
	if(consume(DOT)){
		if(consume(ID)){
			if(exprPostfixPrim()){
				return true;
				}
			} else tkerr("Missing struct attribute after .");
		}
	return true;
}

bool exprPostfix() {
	printf("#exprPostfix %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprPrimary()){
		if(exprPostfixPrim()){
			return true;
			}
		}
	iTk = start;
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix

bool exprUnary() {
	printf("#exprUnary %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(SUB) || consume(NOT)){
		if(exprUnary()){
			return true;
			} else {
				iTk = start;
				if (consume(NOT)) tkerr("Missing expression after !");
			}
		}
	iTk = start;
	if (exprPostfix()) {
		return true;
	}
	iTk = start;
	return false;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary

bool exprCast() {
	printf("#exprCast %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LPAR)){
		if(typeBase()){
			if(arrayDecl()){}
				if(consume(RPAR)){
					if(exprCast()){
						return true;
						} else tkerr("Missing expression after cast");
					} else tkerr("Missing ) after type");
			}
		}
	iTk = start;
	if ( exprUnary() ) {
		return true;
	}
	iTk = start;
	return false;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
// transformed into exprMul: exprCast exprMulPrim, exprMulPrim: ((MUL | DIV) exprCast exprMulPrim)?

bool exprMulPrim() {
	printf("#exprMulPrim %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(MUL) || consume(DIV)){
		if(exprCast()){
			if(exprMulPrim()){
				return true;
				}
			} else {
				iTk = start;
				if (consume(MUL)) tkerr("Missing expression after *");
				else if (consume(DIV)) tkerr("Missing expression after /");
			}
		}
	iTk = start;
	return true;
}

bool exprMul() {
	printf("#exprMul %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprCast()){
		if(exprMulPrim()){
			return true;
			}
		}
	iTk = start;
	return false;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// transformed into exprAdd: exprMul exprAddPrim, exprAddPrim: ((ADD | SUB) exprMul exprAddPrim)?

bool exprAddPrim() {
	printf("#exprAddPrim %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(ADD) || consume(SUB)){
		if(exprMul()){
			if(exprAddPrim()){
				return true;
				}
			} else {
				iTk = start;
				if (consume(ADD)) tkerr("Missing expression after +");
				else if (consume(SUB)) tkerr("Missing expression after -");
			}
		}
	iTk = start;
	return true;
}

bool exprAdd() {
	printf("#exprADd %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprMul()){
		if(exprAddPrim()){
			return true;
			}
		}
	iTk = start;
	return false;
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// transformed into exprRel: exprAdd exprRelPrim, exprRelPrim: ((LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrim)?

bool exprRelPrim() {
	printf("#exprRelPrim %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)){
		if(exprAdd()){
			if(exprRelPrim()){
				return true;
				}
			} else {
				iTk = start;
				if (consume(LESS)) tkerr("Missing expression after <");
				else if (consume(LESSEQ)) tkerr("Missing expression after <=");
				else if (consume(GREATER)) tkerr("Missing expression after >");
				else if (consume(GREATEREQ)) tkerr("Missing expression after >=");
			}
		}
	iTk = start;
	return true;
}

bool exprRel() {
	printf("#exprRel %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprAdd()){
		if(exprRelPrim()){
			return true;
			}
		}
	iTk = start;
	return false;
}


// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
// transformed into exprEq: exprRel exprEqPrim, exprEqPrim: ((EQUAL | NOTEQ) exprRel exprEqPrim)?

bool exprEqPrim() {
	printf("#exprEqPrim %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(EQUAL) || consume(NOTEQ)){
		if(exprRel()){
			if(exprEqPrim()){
				return true;
				}
			} else tkerr("Missing expression after == or !=");
		}
	iTk = start;
	return true;
}

bool exprEq(){
	printf("#exprEq %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprRel()){
		if(exprEqPrim()){
			return true;
			}
		}
	iTk = start;
	return false;
	}


// exprAnd: exprAnd AND exprEq | exprEq
// transformed into exprAnd: exprEq exprAndPrim, exprAndPrim: (AND exprEq exprAndPrim)?

bool exprAndPrim() {
	printf("#exprAndPrim %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(AND)){
		if(exprEq()){
			if(exprAndPrim()){
				return true;
				}
			} else tkerr("Missing expression after &&");
		}
	iTk = start;
	return true;
}

bool exprAnd(){
	printf("#exprAnd %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprEq()){
		if(exprAndPrim()){
			return true;
			}
		}
	iTk = start;
	return false;
	}

// exprOr: exprOr OR exprAnd | exprAnd
// transformed into exprOr: exprAnd exprOrPrim, exprOrPrim: (OR exprAnd exprOrPrim)?

bool exprOrPrim() {
	printf("#exprOrPrim %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(OR)){
		if(exprAnd()){
			if(exprOrPrim()){
				return true;
				}
			} else tkerr("Missing expression after ||");
		}
	iTk = start;
	return true;
}

bool exprOr(){
	printf("#exprOr %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprAnd()){
		if(exprOrPrim()){
			return true;
			}
		}
	iTk = start;
	return false;
	}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr

bool exprAssign(){
	printf("#exprAssign %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprOr()){
		if(consume(ASSIGN)){
			if(exprAssign()){
				return true;
				} else tkerr("Missing expression after =");
			}
		}
	iTk = start;
	if (exprOr()){
		return true;
		}
	return false;
	}


// expr: exprAssign

bool expr(){
	printf("#expr %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprAssign()){
		return true;
		}
	iTk = start;
	return false;
	}

// stmCompound: LACC ( varDef | stm )* RACC

bool stmCompound(){
	printf("#stmCompound %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LACC)){
		while(varDef() || stm());
		if(consume(RACC)){
			return true;
			} else tkerr("Syntax error in started code block");
		}
	iTk = start;
	return false;
	}

// stm: stmCompound | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON

bool stm(){
	printf("#stm %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(stmCompound()){
		return true;
		}
	if(consume(IF)){
		if(consume(LPAR)){
			if(expr()){
				if(consume(RPAR)){
					if(stm()){
						if(consume(ELSE)){
							if(stm()){
								return true;
								}
							}else{
								return true;
								}
						} else tkerr("Missing statement in if");
					} else tkerr("Missing ) after expression in if");
				} else tkerr("Missing condition in if");
			} else tkerr("Missing ( after if");
		}
	iTk = start;
	if(consume(WHILE)){
		if(consume(LPAR)){
			if(expr()){
				if(consume(RPAR)){
					if(stm()){
						return true;
						} else tkerr("Missing statement in while");
					} else tkerr("Missing ) after expression in while");
				} else tkerr("Missing expression in while");
			} else tkerr("Missing ( after while");
		}
	iTk = start;
	if(consume(RETURN)){
		expr();
		if(consume(SEMICOLON)){
			return true;
			} else tkerr("Missing ; after return");
		}
	iTk = start;
	if (expr()) {
		if (consume(SEMICOLON)) {
			return true;
			} else tkerr("Missing ; after expression");
		}
	iTk = start;
	if(consume(SEMICOLON)){
		return true;
		} 
	return false;
	}

// fnParam: typeBase ID arrayDecl?

bool fnParam(){
	printf("#fnParam %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(typeBase()){
		if(consume(ID)){
			if (arrayDecl()) {}
			return true;
			}
		}
	iTk=start;
	return false;
	}


// fnDef: ( typeBase | VOID ) ID LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound

bool fnDef(){
	printf("#fnDef %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(typeBase() || consume(VOID)){
		if(consume(ID)){
			if(consume(LPAR)){
				if(fnParam()){
						for (;;) {
							if (consume(COMMA)) {
								if (fnParam()) {
									continue;
								} else tkerr("Missing function parameter after comma");
							}
							break;
						}
					}
				if(consume(RPAR)){
					if(stmCompound()){
						return true;
						} else tkerr("Missing function body");
					} else tkerr("Missing ) after function parameters");
				} 
			}
		}
	iTk=start;
	return false;
	}


// arrayDecl: LBRACKET INT? RBRACKET

bool arrayDecl(){
	printf("#arrayDecl %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LBRACKET)){
		if(consume(INT)){}
		if(consume(RBRACKET)){
			return true;
			} else tkerr("Missing ] after array declaration");
		}
	iTk=start;
	return false;
	}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(){
	printf("#typeBase %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(TYPE_INT)){
		return true;
		}
	if(consume(TYPE_DOUBLE)){
		return true;
		}
	if(consume(TYPE_CHAR)){
		return true;
		}
	if(consume(STRUCT)){
		if(consume(ID)){
			return true;
			}
		}
	iTk=start;
	return false;
	}

// varDef: typeBase ID arrayDecl? SEMICOLON

bool varDef(){
	printf("#varDef %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(typeBase()){
		if(consume(ID)){
			if(arrayDecl()){}
			if(consume(SEMICOLON)){
				return true;
				} else tkerr("Missing ; after variable declaration");
			}
		}
	iTk=start;
	return false;
	}

// structDef: STRUCT ID LACC varDef* RACC SEMICOLON

bool structDef(){
	printf("#structDef %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(STRUCT)){
		if(consume(ID)){
			if(consume(LACC)){
				for(;;){
					if(varDef()){}
					else break;
					}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
						return true;
						} else tkerr("Missing ; after struct");
					} else tkerr("Missing } after struct");
				}
			} else tkerr("Missing struct name");
		}
	iTk=start;
	return false;
	}

// unit: ( structDef | fnDef | varDef )* END
bool unit(){
	for(;;){
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;
		}
	if(consume(END)){
		return true;
		}
	return false;
	}

void parse(Token *tokens){
	iTk=tokens; // iTk - iterator in atom list
	if(!unit())tkerr("syntax error");
	
	}

char* tkCode(int code) {
	char* s = safeAlloc(10 * sizeof(char));
	if (code == ID) strcpy(s, "ID");
	else if (code == INT) strcpy(s, "INT");
	else if (code == DOUBLE) strcpy(s, "DOUBLE");
	else if (code == CHAR) strcpy(s, "CHAR");
	else if (code == STRING) strcpy(s, "STRING");
	else if (code == STRUCT) strcpy(s, "STRUCT");
	else if (code == TYPE_INT) strcpy(s, "TYPE_INT");
	else if (code == TYPE_DOUBLE) strcpy(s, "TYPE_DOUBLE");
	else if (code == TYPE_CHAR) strcpy(s, "TYPE_CHAR");
	else if (code == LPAR) strcpy(s, "LPAR");
	else if (code == RPAR) strcpy(s, "RPAR");
	else if (code == LACC) strcpy(s, "LACC");
	else if (code == RACC) strcpy(s, "RACC");
	else if (code == LBRACKET) strcpy(s, "LBRACKET");
	else if (code == RBRACKET) strcpy(s, "RBRACKET");
	else if (code == COMMA) strcpy(s, "COMMA");
	else if (code == SEMICOLON) strcpy(s, "SEMICOLON");
	else if (code == DOT) strcpy(s, "DOT");
	else if (code == ASSIGN) strcpy(s, "ASSIGN");
	else if (code == IF) strcpy(s, "IF");
	else if (code == ELSE) strcpy(s, "ELSE");
	else if (code == WHILE) strcpy(s, "WHILE");
	else if (code == RETURN) strcpy(s, "RETURN");
	else if (code == VOID) strcpy(s, "VOID");
	else if (code == END) strcpy(s, "END");
	else if (code == ADD) strcpy(s, "ADD");
	else if (code == SUB) strcpy(s, "SUB");
	else if (code == MUL) strcpy(s, "MUL");
	else if (code == DIV) strcpy(s, "DIV");
	else if (code == NOT) strcpy(s, "NOT");
	else if (code == OR) strcpy(s, "OR");
	else if (code == AND) strcpy(s, "AND");
	else if (code == EQUAL) strcpy(s, "EQUAL");
	else if (code == LESS) strcpy(s, "LESS");
	else if (code == GREATER) strcpy(s, "GREATER");
	else if (code == GREATEREQ) strcpy(s, "GREATEREQ");
	else if (code == LESSEQ) strcpy(s, "LESSEQ");
	else if (code == NOTEQ) strcpy(s, "NOTEQ");
	else strcpy(s, "UNKNOWN");
	
}