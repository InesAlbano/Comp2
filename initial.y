%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "tabid.h"
#include "node.h"
extern int yylex();

int yylex(); /* -Wall */
static char buf[80]; /* error messages */
static int gt, pos, dim(Node*);
static void *gtr, *root;
static Node *name(Node*);
%}
%union {
	int    i;		/* integer value */
	double r;		/* real value */
	char  *s;		/* symbol name or string literal */
	Node  *n;   /* tree node */
}

%token <i> INT
%token <r> REAL
%token <s> ID STR
%token DO WHILE IF THEN FOR IN UPTO DOWNTO STEP BREAK CONTINUE
%token VOID INTEGER STRING NUMBER CONST PUBLIC INCR DECR
%token ATR NE GE LE ELSE

%nonassoc IFX
%nonassoc ELSE
%right 		ATR
%left 	 	'|'
%left 	 	'&'
%nonassoc '~'
%left  		'=' NE
%left  		'<' '>' LE GE
%left 		'+' '-'
%left 		'*' '/' "%"
%nonassoc	'!' ADDR UMINUS
%nonassoc '[' '('



%%
fich : decl
		 ;

decl : decl def
     |
		 ;

def  : public_option const_option type point_option ID init ';'
		 ;

public_option : PUBLIC
     					|
							;

const_option  : CONST
							|
							;

point_option 	: '*'
		  			 	|
		 			 	 	;

type 	: INTEGER
			| STRING
			| NUMBER
			| VOID
			;


init 	: ATR INT
		 	| ATR const_option STR
		 	| ATR REAL
		 	| ATR ID
		 	| '(' args ')' body_option
		 	|
		 	;


args 	: args ',' param
		 	|
		 	;

param : type point_option ID
			;

body_option : body
						|
						;

body 	 : '{' params instrs '}'
		 	 ;

params : params param ';'
			 |
			 ;

instrs : instrs instr
			 |
			 ;

instr 			: IF expres THEN instr  %prec IFX
						| IF expres THEN instr ELSE instr
      			| DO instr WHILE expres ';'
						| FOR lvalue IN expres to_option expres step_option DO instr // for *i
						| expres ';'
						| body
						| BREAK int_option ';'
						| CONTINUE int_option ';'
						| lvalue '#' expres ';'
						;


to_option 	: UPTO
          	| DOWNTO
						;

step_option : STEP expres
						|
						;

int_option  : INT
						|
						;

lvalue  : ID
				| ID '[' expres ']'
				| '*' ID
				;

expres	: lvalue
				| INT // literal
				| STR
				| REAL
				| lvalue INCR
				| lvalue DECR
				| INCR lvalue
				| DECR lvalue
				| '&' lvalue %prec ADDR
        | '-' expres %prec UMINUS
				| '!' expres
				| '~' expres
        | '(' expres ')'
			  | expres '(' arg_func ')'
				| expres '/' expres
				| expres '%' expres
				| expres '+' expres
				| expres '<' expres
				| expres '>' expres
				| expres GE  expres
				| expres LE  expres
				| expres '=' expres
				| expres NE  expres
				| expres '-' expres
				| expres '*' expres
				| expres '&' expres
				;

arg_func : arg_func ',' expres
				 | expres
				 |
				 ;

%%
int yyerror(char *s) { printf("%s\n",s); return 1; }
char *dupstr(const char*s) { return strdup(s); }
int main(int argc, char *argv[]) {
 extern YYSTYPE yylval;
 int tk;
 while ((tk = yylex()))
  if (tk > YYERRCODE)
   printf("%d:\t%s\n", tk, yyname[tk]);
  else
   printf("%d:\t%c\n", tk, tk);
 return 0;
}
