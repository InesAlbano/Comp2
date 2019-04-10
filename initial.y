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

%token LOCAL

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
		 	| param
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
						| FOR lvalue IN expres to_option expres step_option DO instr
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
				| INT
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
				| lvalue ATR expres
				;

arg_func : arg_func ',' expres
				 | expres
				 |
				 ;

%%
static Node *name(Node *nm) {
  int typ = 2, pos = 0;
  if (!gt)
    typ = IDfind(nm->value.s, (long*)&pos);
  else { /* in a goto statment */
    root = IDroot(gtr);
    IDnew(0, nm->value.s, IDtest);
    gtr = IDroot(root);
    if (IDfind(nm->value.s, (long*)IDtest) < 0)
      typ = 2; /* reference to foward label */
    else
      typ = IDfind(nm->value.s, (long*)&pos);
  }
  /* check types */
  if (pos != 0)
    nm = intNode(LOCAL, pos);
  else
    nm = strNode(ADDR, nm->value.s);
  nm->info = typ;
  return nm;
}

static int dim(Node *n)
{
  if (n->type == nodeNil) return 1;
  return n->value.i;
}

int yyerror(const char*); /* declaration may depend on yacc's version/flavor */
char **yynames =
#if YYDEBUG > 0
		 (char**)yyname;
#else
		 0;
#endif

/* goto auxiliar functions */
static void gtf(int typ, char *name, int attrib, int user) {
  gtr = IDroot(root);
  if ((typ = IDsearch(name, 0, 0, 1)) != -1 && typ != 2)
    yyerror(strcat(strcpy(buf, name),": not a label"));
  root = IDroot(gtr);
}

static void gotos() {
  root = IDroot(gtr);
  IDforall((IDfunc)gtf, 0, 0, 0);
  IDclear();
  gtr = IDroot(root);
}
