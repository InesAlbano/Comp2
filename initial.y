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

%type<n> fich decl def public_option const_option point_option type init args param body_option body params instr instrs
%type<n> to_option step_option int_option lvalue expres arg_func



%token LOCAL NIL PROG DECL IINT ISTR IREAL IID ARGS PARAM EXPRES ALLOC LVALUE LVID LVIDE POINTER BODY PARAMS TO TYPE
%token PTR UMINUS FACTR FACTL NOT FCALL DIV MOD PLUS LT GT GE LE EQ NE SUBX NUL AND OR ATR ARG

%%
/*{$$ = printNode($1, 0, yynames);}*/
fich : decl 		 {printNode($1, 0, yynames);}
		 ;

decl : decl def  {$$ = binNode(DECL, $1, $2);}
     | 					 {$$ = nilNode(NIL);}
		 ;

def  : public_option const_option type point_option ID init ';' {$$ = binNode(PUBLIC, $1, binNode(CONST, $2, binNode(TYPE, $3, binNode(POINTER, $4, binNode(ID, strNode(ID, $5), $6)))));}
		 ;

public_option : PUBLIC {$$ = nilNode(PUBLIC);}
     					| 			 {$$ = nilNode(NIL);}
							;

const_option  : CONST  {$$ = nilNode(CONST);}
							| 		   {$$ = nilNode(NIL);}
							;

point_option 	: '*'    {$$ = nilNode(POINTER);}
		  			 	| 		   {$$ = nilNode(NIL);}
		 			 	 	;

type 	: INTEGER 			 {$$ = nilNode(INTEGER);}
			| STRING 				 {$$ = nilNode(STRING);}
			| NUMBER 				 {$$ = nilNode(NUMBER);}
			| VOID 					 {$$ = nilNode(VOID);}
			;


init 	: ATR INT   								{$$ = intNode(IINT,  $2);}
		 	| ATR const_option STR			{$$ = strNode(ISTR,  $3);}
		 	| ATR REAL									{$$ = realNode(IREAL,$2);}
		 	| ATR ID										{$$ = uniNode(IID,   strNode(ID, $2));}
		 	| '(' args ')' body_option	{$$ = binNode(IINT,  $2, $4);}
		 	| 													{$$ = nilNode(NIL);}
		 	;


args 	: args ',' param						{$$ = binNode(ARGS, $1, $3);}
		 	| param											{$$ = binNode(ARGS, nilNode(NIL), $1);}
			| 													{$$ = nilNode(NIL);}
		 	;

//TODO string *
param : type point_option ID      {$$ = binNode(PARAM, $1, strNode(ID, $3));}
			;

body_option : body								{$$ = $1;}
						| 										{$$ = nilNode(NIL);}
						;

body 	 : '{' params instrs '}'		{$$ = binNode(BODY, $2, $3);}
		 	 ;

params : params param ';'					{$$ = binNode(PARAMS, $1, $2);}
			 | 													{$$ = nilNode(NIL);}
			 ;

instrs : instrs instr							{$$ = binNode(PARAMS, $1, $2);}
			 | 											    {$$ = nilNode(NIL);}
			 ;

instr 			: FOR lvalue IN expres to_option expres step_option DO instr {$$ = binNode(FOR, $2, binNode(IN, $4, binNode(TO, $5, binNode(EXPRES, $6, binNode(STEP, $7, $9)))));}
						| IF expres THEN instr  %prec IFX  {$$ = binNode(IF,      $2, 							   $4);}
						| IF expres THEN instr ELSE instr  {$$ = binNode(ELSE,    binNode(IF, $2, $4), $6);}
      			| DO instr WHILE expres ';'				 {$$ = binNode(DO,      $2,   						   binNode(WHILE, $2, $4));}
						| expres ';'											 {$$ = $1;}
						| body                             {$$ = $1;}
						| BREAK int_option ';'						 {$$ = uniNode(BREAK,    $2);}
						| CONTINUE int_option ';'					 {$$ = uniNode(CONTINUE, $2);}
						| lvalue '#' expres ';'						 {$$ = binNode(ALLOC,    $1, $3);}
						;


to_option 	: UPTO 					{$$ = nilNode(NIL);}
          	| DOWNTO				{$$ = nilNode(NIL);}
						;

step_option : STEP expres		{$$ = binNode(STEP,   nilNode(NIL), $2);}
						| 							{$$ = nilNode(NIL);}
						;

int_option  : INT						{$$ = intNode(INT,    $1);}
						| 							{$$ = nilNode(NIL);}
						;

lvalue  : ID								{$$ = uniNode(LVID,   strNode(ID, $1));}
				| ID '[' expres ']' {$$ = binNode(LVIDE,  strNode(ID, $1), $3);}
				| '*' ID						{$$ = uniNode(LVALUE, strNode(ID, $2));}
				;

expres	: lvalue					  {$$ = uniNode(LVALUE, $1);}
				| INT								{$$ = intNode(INT,    $1);}
				| STR								{$$ = strNode(STR,    $1);}
				| REAL							{$$ = realNode(REAL,  $1);}
				| lvalue INCR				{$$ = $1;}//TODO
				| lvalue DECR				{$$ = $1;}//TODO
				| INCR lvalue				{$$ = $2;}//TODO
				| DECR lvalue				{$$ = $2;}//TODO
				| '(' expres ')'					{$$ = $2;}
				| '&' lvalue %prec ADDR   {$$ = uniNode(PTR,    $2);}
        | '-' expres %prec UMINUS {$$ = uniNode(UMINUS, $2);}
				| '!' expres							{$$ = uniNode(FACTR,  $2);}
				| expres '!'							{$$ = uniNode(FACTL,  $1);}
				| '~' expres							{$$ = uniNode(NOT,    $2);}
			  | expres '(' arg_func ')' {$$ = binNode(FCALL,  $1, $3);}
				| expres '/' expres				{$$ = binNode(DIV,  	$1, $3);}
				| expres '%' expres				{$$ = binNode(MOD,  	$1, $3);}
				| expres '+' expres				{$$ = binNode(PLUS, 	$1, $3);}
				| expres '<' expres				{$$ = binNode(LT,   	$1, $3);}
				| expres '>' expres				{$$ = binNode(GT,   	$1, $3);}
				| expres GE  expres				{$$ = binNode(GE,   	$1, $3);}
				| expres LE  expres				{$$ = binNode(LE,   	$1, $3);}
				| expres '=' expres				{$$ = binNode(EQ,     $1, $3);}
				| expres NE  expres				{$$ = binNode(NE,     $1, $3);}
				| expres '-' expres				{$$ = binNode(SUBX,    $1, $3);}
				| expres '*' expres				{$$ = binNode(NUL,    $1, $3);}
				| expres '&' expres				{$$ = binNode(AND,    $1, $3);}
				| expres '|' expres				{$$ = binNode(OR,     $1, $3);}
				| lvalue ATR expres				{$$ = binNode(ATR,    $1, $3);}
				;

arg_func : arg_func ',' expres		{$$ = binNode(ARG, $1, $3);}
				 | expres									{$$ = binNode(ARG, $1, nilNode(NIL));}
				 | 												{$$ = nilNode(NIL);}
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
