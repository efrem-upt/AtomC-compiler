#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include "../utils/utils.h"

#include "parser.h"
#include "ad.h"
#include "at.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

Symbol* owner = NULL;	// the current owner of the symbols

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

bool exprPrimary(Ret* r) {
	printf("#exprPrimary %s\n",tkCode(iTk->code));
	Token* start = iTk;
	if(consume(ID)){
		Token* tkName = consumedTk;
		Symbol *s=findSymbol(tkName->text);
		if(!s)tkerr("undefined id: %s",tkName->text);
		if(consume(LPAR)){
			if(s->kind!=SK_FN)tkerr("only a function can be called");
			Ret rArg;
			Symbol *param=s->fn.params;
			if(consume(RPAR)){
				return true;
				}
			if(expr(&rArg)){
				if(!param)tkerr("too many arguments in function call");
				if(!convTo(&rArg.type,&param->type))tkerr("in call, cannot convert the argument type to the parameter type");
				param=param->next;
				while(consume(COMMA)){
					if(expr(&rArg)){
						if(!param)tkerr("too many arguments in function call");
						if(!convTo(&rArg.type,&param->type))tkerr("in call, cannot convert the argument type to the parameter type");
						param=param->next;
						continue;
						} else tkerr("Missing function argument after ,");
					iTk = start;
					return false;
					}
				if(consume(RPAR)){
					if(param)tkerr("too few arguments in function call");
					*r=(Ret){s->type,false,true};
					return true;
					} else tkerr("Missing ) to end function call");
				}
			} else {
				if(s->kind==SK_FN)tkerr("a function can only be called");
				*r=(Ret){s->type,true,s->type.n>=0};
			}
		return true;
		}
	iTk = start;
	if(consume(INT)) {
		*r=(Ret){{TB_INT,NULL,-1},false,true};
		return true;
	}
	if (consume(DOUBLE)) {
		*r=(Ret){{TB_DOUBLE,NULL,-1},false,true};
		return true;
	}
	if (consume(CHAR)) {
		*r=(Ret){{TB_CHAR,NULL,-1},false,true};
		return true;
	}
	if (consume(STRING)) {
		*r=(Ret){{TB_CHAR,NULL,0},false,true};
		return true;
	}

	if(consume(LPAR)){
		if(expr(r)){
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

bool exprPostfixPrim(Ret* r) {
	printf("#exprPostfixPrim %s\n",tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LBRACKET)){
		Ret idx;
		if(expr(&idx)){
			if(consume(RBRACKET)){
				if(r->type.n<0)tkerr("only an array can be indexed");
				Type tInt={TB_INT,NULL,-1};
				if(!convTo(&idx.type,&tInt))tkerr("the index is not convertible to int");
				r->type.n=-1;
				r->lval=true;
				r->ct=false;
				if(exprPostfixPrim(r)){
					return true;
					}
				} else tkerr("Missing ] after expression");
			}
		}
	iTk = start;
	if(consume(DOT)){
		if(consume(ID)){
			Token* tkName = consumedTk;
			if(r->type.tb!=TB_STRUCT)tkerr("a field can only be selected from a struct");
			Symbol *s=findSymbolInList(r->type.s->structMembers,tkName->text);
			if(!s)tkerr("the structure %s does not have a field %s",r->type.s->name,tkName->text);
			*r=(Ret){s->type,true,s->type.n>=0};
			if(exprPostfixPrim(r)){
				return true;
				}
			} else tkerr("Missing struct attribute after .");
		}
	return true;
}

bool exprPostfix(Ret* r) {
	printf("#exprPostfix %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprPrimary(r)){
		if(exprPostfixPrim(r)){
			return true;
			}
		}
	iTk = start;
	return false;
}

// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix

bool exprUnary(Ret* r) {
	printf("#exprUnary %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(SUB) || consume(NOT)){
		if(exprUnary(r)){
			if(!canBeScalar(r))tkerr("unary - or ! must have a scalar operand");
			r->lval=false;
			r->ct=true;
			return true;
			} else {
				iTk = start;
				if (consume(NOT)) tkerr("Missing expression after !");
			}
		}
	iTk = start;
	if (exprPostfix(r)) {
		return true;
	}
	iTk = start;
	return false;
}

// exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary

bool exprCast(Ret* r) {
	printf("#exprCast %s\n", tkCode(iTk->code));
	Token* start = iTk;
	Type t;
	if(consume(LPAR)){
		Ret op;
		if(typeBase(&t)){
			if(arrayDecl(&t)){}
				if(consume(RPAR)){
					if(exprCast(&op)){
						if(t.tb==TB_STRUCT)tkerr("cannot convert to a struct type");
						if(op.type.tb==TB_STRUCT)tkerr("cannot convert a struct");
						if(op.type.n>=0&&t.n<0)tkerr("an array can be converted only to another array");
						if(op.type.n<0&&t.n>=0)tkerr("a scalar can be converted only to another scalar");
						*r=(Ret){t,false,true};
						return true;
						} else tkerr("Missing expression after cast");
					} else tkerr("Missing ) after type");
			}
		}
	iTk = start;
	if ( exprUnary(r) ) {
		return true;
	}
	iTk = start;
	return false;
}

// exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
// transformed into exprMul: exprCast exprMulPrim, exprMulPrim: ((MUL | DIV) exprCast exprMulPrim)?

bool exprMulPrim(Ret* r) {
	printf("#exprMulPrim %s\n", tkCode(iTk->code));
	Ret right;
	Token* start = iTk;
	if(consume(MUL) || consume(DIV)){
		if(exprCast(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for * or /");
			*r=(Ret){tDst,false,true};
			if(exprMulPrim(r)){
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

bool exprMul(Ret* r) {
	printf("#exprMul %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprCast(r)){
		if(exprMulPrim(r)){
			return true;
			}
		}
	iTk = start;
	return false;
}

// exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
// transformed into exprAdd: exprMul exprAddPrim, exprAddPrim: ((ADD | SUB) exprMul exprAddPrim)?

bool exprAddPrim(Ret* r) {
	printf("#exprAddPrim %s\n", tkCode(iTk->code));
	Ret right;
	Token* start = iTk;
	if(consume(ADD) || consume(SUB)){
		if(exprMul(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for + or -");
			*r=(Ret){tDst,false,true};
			if(exprAddPrim(r)){
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

bool exprAdd(Ret* r) {
	printf("#exprADd %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprMul(r)){
		if(exprAddPrim(r)){
			return true;
			}
		}
	iTk = start;
	return false;
}

// exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
// transformed into exprRel: exprAdd exprRelPrim, exprRelPrim: ((LESS | LESSEQ | GREATER | GREATEREQ) exprAdd exprRelPrim)?

bool exprRelPrim(Ret* r) {
	printf("#exprRelPrim %s\n", tkCode(iTk->code));
	Ret right;
	Token* start = iTk;
	if(consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)){
		if(exprAdd(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for <, <=, >, >=");
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprRelPrim(r)){
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

bool exprRel(Ret* r) {
	printf("#exprRel %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprAdd(r)){
		if(exprRelPrim(r)){
			return true;
			}
		}
	iTk = start;
	return false;
}


// exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
// transformed into exprEq: exprRel exprEqPrim, exprEqPrim: ((EQUAL | NOTEQ) exprRel exprEqPrim)?

bool exprEqPrim(Ret* r) {
	printf("#exprEqPrim %s\n", tkCode(iTk->code));
	Ret right;
	Token* start = iTk;
	if(consume(EQUAL) || consume(NOTEQ)){
		if(exprRel(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for == or !=");
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprEqPrim(r)){
				return true;
				}
			} else tkerr("Missing expression after == or !=");
		}
	iTk = start;
	return true;
}

bool exprEq(Ret* r){
	printf("#exprEq %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprRel(r)){
		if(exprEqPrim(r)){
			return true;
			}
		}
	iTk = start;
	return false;
	}


// exprAnd: exprAnd AND exprEq | exprEq
// transformed into exprAnd: exprEq exprAndPrim, exprAndPrim: (AND exprEq exprAndPrim)?

bool exprAndPrim(Ret* r) {
	printf("#exprAndPrim %s\n", tkCode(iTk->code));
	Ret right;
	Token* start = iTk;
	if(consume(AND)){
		if(exprEq(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for &&");
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprAndPrim(r)){
				return true;
				}
			} else tkerr("Missing expression after &&");
		}
	iTk = start;
	return true;
}

bool exprAnd(Ret* r){
	printf("#exprAnd %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprEq(r)){
		if(exprAndPrim(r)){
			return true;
			}
		}
	iTk = start;
	return false;
	}

// exprOr: exprOr OR exprAnd | exprAnd
// transformed into exprOr: exprAnd exprOrPrim, exprOrPrim: (OR exprAnd exprOrPrim)?

bool exprOrPrim(Ret* r) {
	printf("#exprOrPrim %s\n", tkCode(iTk->code));
	Ret right;
	Token* start = iTk;
	if(consume(OR)){
		if(exprAnd(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst))tkerr("invalid operand type for ||");
			*r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprOrPrim(r)){
				return true;
				}
			} else tkerr("Missing expression after ||");
		}
	iTk = start;
	return true;
}

bool exprOr(Ret* r){
	printf("#exprOr %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprAnd(r)){
		if(exprOrPrim(r)){
			return true;
			}
		}
	iTk = start;
	return false;
	}

// exprAssign: exprUnary ASSIGN exprAssign | exprOr

bool exprAssign(Ret* r){
	printf("#exprAssign %s\n", tkCode(iTk->code));
	Ret rDst;
	Token* start = iTk;
	if(exprUnary(&rDst)){
		if(consume(ASSIGN)){
			if(exprAssign(r)){
				if(!rDst.lval)tkerr("the assign destination must be a left-value");
				if(rDst.ct)tkerr("the assign destination cannot be constant");
				if(!canBeScalar(&rDst))tkerr("the assign destination must be scalar");
				if(!canBeScalar(r))tkerr("the assign source must be scalar");
				if(!convTo(&r->type,&rDst.type))tkerr("the assign source cannot be converted to destination");
				r->lval=false;
				r->ct=true;
				return true;
				} else tkerr("Missing expression after =");
			}
		}
	iTk = start;
	if (exprOr(r)){
		return true;
		}
	return false;
	}


// expr: exprAssign

bool expr(Ret* r){
	printf("#expr %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(exprAssign(r)){
		return true;
		}
	iTk = start;
	return false;
	}

// stmCompound: LACC ( varDef | stm )* RACC

bool stmCompound(bool newDomain){
	printf("#stmCompound %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LACC)){
		if(newDomain)pushDomain();
		while(varDef() || stm());
		if(consume(RACC)){
			if(newDomain) dropDomain();
			return true;
			} else tkerr("Syntax error in started code block");
		}
	iTk = start;
	return false;
	}

// stm: stmCompound | IF LPAR expr RPAR stm ( ELSE stm )? | WHILE LPAR expr RPAR stm | RETURN expr? SEMICOLON | expr? SEMICOLON

bool stm(){
	printf("#stm %s\n", tkCode(iTk->code));
	Ret rCond, rExpr;
	Token* start = iTk;
	if(stmCompound(true)){
		return true;
		}
	if(consume(IF)){
		if(consume(LPAR)){
			if(expr(&rCond)){
				if(!canBeScalar(&rCond))tkerr("the if condition must be a scalar value");
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
			if(expr(&rCond)){
				if(!canBeScalar(&rCond))tkerr("the while condition must be a scalar value");
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
		if (expr(&rExpr)) {
			if(owner->type.tb==TB_VOID)tkerr("a void function cannot return a value");
			if(!canBeScalar(&rExpr))tkerr("the return value must be a scalar value");
			if(!convTo(&rExpr.type,&owner->type))tkerr("cannot convert the return expression type to the function return type");
		} else {
			if(owner->type.tb!=TB_VOID)tkerr("a non-void function must return a value");
		}
		if(consume(SEMICOLON)){
			return true;
			} else tkerr("Missing ; after return");
		}
	iTk = start;
	if (expr(&rExpr)) {
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
	Type t;
	Token* start = iTk;
	if(typeBase(&t)){
		if(consume(ID)){
			Token* tkName = consumedTk;
			if (arrayDecl(&t)) {t.n = 0;}
			Symbol *param=findSymbolInDomain(symTable,tkName->text);
			if(param)tkerr("symbol redefinition: %s",tkName->text);
			param=newSymbol(tkName->text,SK_PARAM);
			param->type=t;
			param->owner=owner;
			param->paramIdx=symbolsLen(owner->fn.params);
			// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
			addSymbolToDomain(symTable,param);
			addSymbolToList(&owner->fn.params,dupSymbol(param));
			return true;
			}
		}
	iTk=start;
	return false;
	}


// fnDef: ( typeBase | VOID ) ID LPAR ( fnParam ( COMMA fnParam )* )? RPAR stmCompound

bool fnDef(){
	printf("#fnDef %s\n", tkCode(iTk->code));
	Type t;
	Token* start = iTk;
	if(typeBase(&t) || consume(VOID)){
		if (start->code == VOID) 
			t.tb = TB_VOID;
		if(consume(ID)){
			Token* tkName = consumedTk;
			if(consume(LPAR)){
				Symbol *fn=findSymbolInDomain(symTable,tkName->text);
				if(fn)tkerr("symbol redefinition: %s",tkName->text);
				fn=newSymbol(tkName->text,SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable,fn);
				owner=fn;
				pushDomain();
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
					if(stmCompound(false)){
						dropDomain();
						owner = NULL;
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

bool arrayDecl(Type* t){
	printf("#arrayDecl %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(LBRACKET)){
		if(consume(INT)){
			Token* tkSize = consumedTk;
			t->n = tkSize->i;
		} else {
			t->n = 0;
		}
		if(consume(RBRACKET)){
			return true;
			} else tkerr("Missing ] after array declaration");
		}
	iTk=start;
	return false;
	}

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type* t){
	t->n = -1;
	printf("#typeBase %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(consume(TYPE_INT)){
		t->tb = TB_INT;
		return true;
		}
	if(consume(TYPE_DOUBLE)){
		t->tb = TB_DOUBLE;
		return true;
		}
	if(consume(TYPE_CHAR)){
		t->tb = TB_CHAR;
		return true;
		}
	if(consume(STRUCT)){
		if(consume(ID)){
			Token* tkName = consumedTk;
			t->tb=TB_STRUCT;
			t->s=findSymbol(tkName->text);
			if(!t->s)tkerr("structura nedefinita: %s",tkName->text);
			return true;
			}
		}
	iTk=start;
	return false;
	}

// varDef: typeBase ID arrayDecl? SEMICOLON

bool varDef(){
	Type t;
	printf("#varDef %s\n", tkCode(iTk->code));
	Token* start = iTk;
	if(typeBase(&t)){
		if(consume(ID)){
			Token* tkName = consumedTk;
			if(arrayDecl(&t)){
				if (t.n == 0)
					tkerr("a vector variable must have a specified dimension");
			}
			if(consume(SEMICOLON)){
				Symbol *var=findSymbolInDomain(symTable,tkName->text);
				if(var)tkerr("symbol redefinition: %s",tkName->text);
				var=newSymbol(tkName->text,SK_VAR);
				var->type=t;
				var->owner=owner;
				addSymbolToDomain(symTable,var);
				if(owner){
					switch(owner->kind){
						case SK_FN:
							var->varIdx=symbolsLen(owner->fn.locals);
							addSymbolToList(&owner->fn.locals,dupSymbol(var));
							break;
						case SK_STRUCT:
							var->varIdx=typeSize(&owner->type);
							addSymbolToList(&owner->structMembers,dupSymbol(var));
							break;
					}
				}else{
					var->varMem=safeAlloc(typeSize(&t));
				}
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
			Token* tkName = consumedTk; // ID[tkName]
			if(consume(LACC)){
				Symbol *s=findSymbolInDomain(symTable,tkName->text);
				if(s)tkerr("symbol redefinition: %s",tkName->text);
				s=addSymbolToDomain(symTable,newSymbol(tkName->text,SK_STRUCT));
				s->type.tb=TB_STRUCT;
				s->type.s=s;
				s->type.n=-1;
				pushDomain();
				owner=s;
				for(;;){
					if(varDef()){}
					else break;
					}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
						owner=NULL;
						dropDomain();
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