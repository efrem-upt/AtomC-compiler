#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lexer.h"
#include "../utils/utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line

Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
	}

char *extract(const char *begin,const char *end){
	// needs to extract a character sequence from begin to end
		char* text = (char*)safeAlloc(end-begin+1);
		for(int i=0;i<end-begin;i++){
			text[i]=begin[i];
		}
		text[end-begin]='\0';
		return text;
	}

Token *tokenize(const char *pch){
	// pch - current character
	const char *start;
	Token *tk;
	for(;;){ 
		switch(*pch){ // testing current character of each token
			case ' ':case '\t':pch++;break;
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')pch++;
				// fallthrough to \n
			case '\n':
				line++;
				pch++;
				break;
			case '\0':addTk(END);return tokens;
			case ',':addTk(COMMA);pch++;break;
			case ';':addTk(SEMICOLON);pch++;break;
			case '(':addTk(LPAR);pch++;break;
			case ')':addTk(RPAR);pch++;break;
			case '[':addTk(LBRACKET);pch++;break;
			case ']':addTk(RBRACKET);pch++;break;
			case '{':addTk(LACC);pch++;break;
			case '}':addTk(RACC);pch++;break;
			case '+':addTk(ADD);pch++;break;
			case '-':addTk(SUB);pch++;break;
			case '*':addTk(MUL);pch++;break;
			case '/': // check if it is a comment
				if(pch[1]=='/'){
					for(pch+=2;*pch!='\r'&&*pch!='\n'&&*pch!='\0';pch++){}
					}else{
					addTk(DIV);
					pch++;
					}
				break;
			case '.':addTk(DOT);pch++;break;
			case '&': // checking if it is a double character
				if(pch[1]=='&'){
					addTk(AND);
					pch+=2;
					}else{
					err("invalid char: %c (%d)",*pch,*pch);
					}
				break;
			case '|': // checking if it is a double character
				if(pch[1]=='|'){
					addTk(OR);
					pch+=2;
					}else{
					err("invalid char: %c (%d)",*pch,*pch);
					}
				break;
			case '!': // checking if it is a double character
				if(pch[1]=='='){
					addTk(NOTEQ);
					pch+=2;
					}else{
					addTk(NOT);
					pch++;
					}
				break;
			case '=':
				if(pch[1]=='='){
					addTk(EQUAL);
					pch+=2;
					}else{
					addTk(ASSIGN);
					pch++;
					}
				break;
			case '<':
				if(pch[1]=='='){
					addTk(LESSEQ);
					pch+=2;
					}else{
					addTk(LESS);
					pch++;
					}
				break;
			case '>':
				if(pch[1]=='='){
					addTk(GREATEREQ);
					pch+=2;
					}else{
					addTk(GREATER);
					pch++;
					}
				break;
			default: // treating undefined situation for atoms that are composed of interval of characters
				if(isalpha(*pch)||*pch=='_'){
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
					char *text=extract(start,pch);
					if(strcmp(text,"char")==0)addTk(TYPE_CHAR);
					else if(strcmp(text,"double")==0 || strcmp(text,"float") == 0)addTk(TYPE_DOUBLE);
					else if(strcmp(text,"else")==0)addTk(ELSE);
					else if(strcmp(text,"if")==0)addTk(IF);
					else if(strcmp(text,"int")==0)addTk(TYPE_INT);
					else if(strcmp(text,"return")==0)addTk(RETURN);
					else if(strcmp(text,"struct")==0)addTk(STRUCT);
					else if(strcmp(text,"void")==0)addTk(VOID);
					else if(strcmp(text,"while")==0)addTk(WHILE);
					else{
						tk=addTk(ID);
						tk->text = text;
					}
			} // check if it's a STRING
			else if (*pch == '"') {
				for(start = ++pch; *pch != '"' && *pch != '\0' && *pch != '\r' && *pch != '\n'; pch++) {}
				if (*pch != '"') {
					char* text = extract(start, pch);
					err("invalid string: %s at line %d", text, line);
				}
				char *text = extract(start, pch);
				tk = addTk(STRING);
				tk->text = text;
				pch++;
			} // check if it's CHAR
			else if (*pch == '\'') {
				if (pch[1] != '\'' && pch[2] == '\'')  {
					tk = addTk(CHAR);
					tk->c = pch[1];
					pch += 3;
				} else {
					err("invalid char constant at line %d", line);
				}
			} 
			// check if it's INT or DOUBLE
			else if (isdigit(*pch)) {
				int isDouble = 0;
				char doubleElements[3] = ".eE";
				int nrAppearencesElements[3] = { 0,0,0 };
				for (start = pch++; isdigit(*pch) || strchr(doubleElements,*pch) != NULL; pch++) {
					if (strchr(doubleElements, *pch) != NULL) {
						isDouble = 1;
						if (*pch == '.') nrAppearencesElements[0]++;
						else if (*pch == 'e' || *pch == 'E') nrAppearencesElements[1]++;
					}
				}
				char *text = extract(start, pch); // extract the integer or double value until e or E (including e or E) if + and - are not used
				char *text2 = NULL; // extract the value after e or E if + and - are used
				int wrong_format_exponential = 1;
				if (*pch == '+' || *pch == '-') { // if the extraction stops at + or - we extract it separately in text2
					if (pch[-1] == 'e' || pch[-1] == 'E') {
						for (start=pch++; isdigit(*pch); pch++) {
							wrong_format_exponential = 0;
						}
						text2 = extract(start,pch);
					} else wrong_format_exponential = 0; 
				} else 
					wrong_format_exponential = 0;
				if (isDouble) {
					if (text2 != NULL) {
						char *text3 = (char*)safeAlloc(strlen(text) + strlen(text2) + 1);
						strcpy(text3, text);
						strcat(text3, text2);
						text = text3;
					}
					if (nrAppearencesElements[0] > 1 || nrAppearencesElements[1] > 1
					|| (wrong_format_exponential && nrAppearencesElements[1] == 1)
					|| (nrAppearencesElements[1] == 1 && (pch[-1] == 'e' || pch[-1] == 'E')))
						err("invalid double: %s at line %d", text, line);

				}
				tk = addTk(isDouble ? DOUBLE : INT);
				if (isDouble)
					tk->d = atof(text);
				else
					tk->i = atoi(text);
			}
			else 
				err("invalid symbol: %c (%d) at line %d",*pch,*pch, line);
		}
	}
}

void showTokens(const Token *tokens) {
	for(const Token *tk=tokens;tk;tk=tk->next){
		// prints the line and the type of the token separated by tab
		printf("%d: ",tk->line);
		printf("\t");
		switch(tk->code){
			case END:printf("END");break;
			case COMMA:printf("COMMA");break;
			case SEMICOLON:printf("SEMICOLON");break;
			case LPAR:printf("LPAR");break;
			case RPAR:printf("RPAR");break;
			case LBRACKET:printf("LBRACKET");break;
			case RBRACKET:printf("RBRACKET");break;
			case LACC:printf("LACC");break;
			case RACC:printf("RACC");break;
			case ADD:printf("ADD");break;
			case SUB:printf("SUB");break;
			case MUL:printf("MUL");break;
			case DIV:printf("DIV");break;
			case DOT:printf("DOT");break;
			case AND:printf("AND");break;
			case OR:printf("OR");break;
			case NOT:printf("NOT");break;
			case NOTEQ:printf("NOTEQ");break;
			case ASSIGN:printf("ASSIGN");break;
			case EQUAL:printf("EQUAL");break;
			case LESS:printf("LESS");break;
			case LESSEQ:printf("LESSEQ");break;
			case GREATER:printf("GREATER");break;
			case GREATEREQ:printf("GREATEREQ");break;
			case TYPE_CHAR:printf("TYPE_CHAR");break;
			case TYPE_INT:printf("TYPE_INT");break;
			case TYPE_DOUBLE:printf("TYPE_DOUBLE");break;
			case STRUCT:printf("STRUCT");break;
			case IF:printf("IF");break;
			case ELSE:printf("ELSE");break;
			case WHILE:printf("WHILE");break;
			case RETURN:printf("RETURN");break;
			case ID:printf("ID: %s",tk->text);break;
			case CHAR:printf("CHAR: %c",tk->c);break;
			case INT:printf("INT: %d",tk->i);break;
			case DOUBLE:printf("DOUBLE: %g",tk->d);break;
			case STRING:printf("STRING: %s",tk->text);break;
			case VOID: printf("VOID");break;
			default:printf("UNKNOWN: %d",tk->code);break;
		}
		printf("\n");
	}
}
