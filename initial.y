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

%type<n> fich decl def public_option const_option type init args param body_option body params instr instrs
%type<n> to_option step_option int_option lvalue expres arg_func init_var

%token LOCAL NIL PROG DECL IINT ISTR IREAL IID ARGS PARAM EXPRES ALLOC LVALUE LVID LVIDE POINTER BODY PARAMS TO TYPE
%token PTR UMINUS FACTR FACTL NOT FCALL DIV MOD PLUS LT GT GE LE EQ NE SUBX NUL AND OR ATR ARG STRINGPTR INTEGERPTR NUMBERPTR INIT PTR
%token INITIDINT INITIDSTR INITIDREAL INITIDAB INITARGBOD INITIDID ARGBOD FUNCTION

%%
fich : decl 		 {printNode($1, 0, yynames);}
		 ;

decl : decl def  {$$ = binNode(DECL, $1, $2);}
     | 					 {$$ = nilNode(NIL);}
		 ;

def  : public_option const_option init ';' 			{$$ = binNode(PUBLIC, $1, binNode(CONST, $2, $3)); /*IDnew($3->info, LEFT_CHILD($4)->value.s, 0);*/}
 		 | public_option const_option init_var ';' 	{$$ = binNode(PUBLIC, $1, binNode(CONST, $2, $3));}
		 ;

public_option : PUBLIC {$$ = nilNode(PUBLIC); 		$$->info = PUBLIC;}
     					| 			 {$$ = nilNode(NIL);}
							;

const_option  : CONST  {$$ = nilNode(CONST);  		$$->info = CONST;}
							| 		   {$$ = nilNode(NIL);}
							;

type 	: INTEGER 			 {$$ = nilNode(INTEGER); 		$$->info = INTEGER;}
			| INTEGER '*'		 {$$ = nilNode(INTEGERPTR); $$->info = INTEGERPTR;}
			| STRING 				 {$$ = nilNode(STRING); 		$$->info = STRING;}
			| STRING '*'		 {$$ = nilNode(STRINGPTR);  $$->info = STRINGPTR;}
			| NUMBER 				 {$$ = nilNode(NUMBER); 		$$->info = NUMBER;}
			| NUMBER '*'		 {$$ = nilNode(NUMBERPTR);  $$->info = NUMBERPTR;}
			| VOID 					 {$$ = nilNode(VOID); 			$$->info = VOID;}
			;

init_var: type ID {$$ = binNode(TYPE, $1, strNode(ID, $2)); IDnew($1->info, $2, 0);}
				;

init 	: type ID ATR INT   								{$$ = binNode(INITIDINT,  strNode(ID, $2), intNode(INT,    $4)); IDnew($1->info, $2, 0); $$->info = INTEGER;}
			| type ID ATR const_option STR			{$$ = binNode(INITIDSTR,  strNode(ID, $2), strNode(STR,    $5)); IDnew($1->info, $2, 0); $$->info = STRING;}
			| type ID ATR REAL									{$$ = binNode(INITIDREAL, strNode(ID, $2), realNode(REAL,  $4)); IDnew($1->info, $2, 0); $$->info = NUMBER;}
			| type ID ATR ID										{$$ = binNode(INITIDID,   strNode(ID, $2), strNode(ID,     $4)); IDnew($1->info, $2, 0); $$->info = IDfind($4, 0);}
			| type ID '(' args {IDpush();} ')'  {IDnew($1->info, $2, 0);} body_option {$$ = binNode(INITIDAB,   strNode(ID, $2), binNode(ARGBOD, $4, $8));} //func
			;


args 	: args ',' param						{$$ = binNode(ARGS, $1, $3);}
		 	| param											{$$ = binNode(ARGS, nilNode(NIL), $1);}
			| 													{$$ = nilNode(NIL);}
		 	;

param : type ID      							{$$ = binNode(PARAM, $1, strNode(ID, $2)); if($1->info == VOID) yyerror("Variable cannot be void."); else IDnew($1->info, $2, 0);}
			;

body_option : body								{$$ = $1; IDpop();}
						| 										{$$ = nilNode(NIL);}
						;

body 	 : '{' params instrs '}'		{$$ = binNode(BODY, $2, $3);}
		 	 ;

params : params param ';'					{$$ = binNode(PARAMS, $1, $2);}
			 | 													{$$ = nilNode(NIL);}
			 | error ';'								{$$ = nilNode(NIL);}
			 ;

instrs : instrs instr							{$$ = binNode(PARAMS, $1, $2);}
			 | 											    {$$ = nilNode(NIL);}
			 ;

instr 			: FOR lvalue IN expres to_option expres step_option DO instr {$$ = binNode(FOR, $2, binNode(IN, $4, binNode(TO, $5, binNode(EXPRES, $6, binNode(STEP, $7, $9)))));}
						| IF expres THEN instr  %prec IFX  													 {$$ = binNode(IF,      $2, 							   $4);}
						| IF expres THEN instr ELSE instr  													 {$$ = binNode(ELSE,    binNode(IF, $2, $4), $6);}
      			| DO instr WHILE expres ';'				 													 {$$ = binNode(DO,      $2,   						   binNode(WHILE, $2, $4));}
						| expres ';'											 													 {$$ = $1;}
						| body                             													 {$$ = $1;}
						| BREAK int_option ';'						 													 {$$ = uniNode(BREAK,    $2);}
						| CONTINUE int_option ';'					 													 {$$ = uniNode(CONTINUE, $2);}
						| lvalue '#' expres ';'						 													 {$$ = binNode(ALLOC,    $1, $3);}
						;


to_option 	: UPTO 								{$$ = nilNode(NIL);}
          	| DOWNTO							{$$ = nilNode(NIL);}
						;

step_option : STEP expres					{$$ = binNode(STEP,   nilNode(NIL), $2);}
						| 										{$$ = nilNode(NIL);}
						;

int_option  : INT									{$$ = intNode(INT,    $1);}
						| 										{$$ = nilNode(NIL);}
						;

lvalue  : ID											{$$ = uniNode(LVID,   strNode(ID, $1)); $$->info = IDfind($1, 0);}
				| ID '[' expres ']' 			{$$ = binNode(LVIDE,  strNode(ID, $1), $3);}
				| '*' ID									{$$ = uniNode(LVALUE, strNode(ID, $2));}
				;

expres	: lvalue					  			{$$ = uniNode(LVALUE, $1); $$->info = $1->info;}
				| INT											{$$ = intNode(INT,    $1); $$->info = INTEGER;}
				| STR											{$$ = strNode(STR,    $1); $$->info = STRING;}
				| REAL										{$$ = realNode(REAL,  $1); $$->info = NUMBER;}
				| '(' expres ')'					{$$ = $2;}
				| expres '(' arg_func ')' {$$ = binNode(FCALL,  $1, $3);}
				| lvalue ATR expres				{$$ = binNode(ATR,    $1, $3); if($1->info == VOID)		 yyerror("Void functions cannot have return values."); else if ($1->info != $3->info) yyerror("Illegal return.");}
				| lvalue INCR							{$$ = uniNode(INCR, 	$1); 		 if($1->info == STRING){ yyerror("Cannot increment other element other than integer or reals."); 																 $$->info = INTEGER;}}
				| lvalue DECR							{$$ = uniNode(DECR, 	$1); 		 if($1->info == STRING){ yyerror("Cannot decrement other element other than integer or reals."); 																 $$->info = INTEGER;}}
				| INCR lvalue							{$$ = uniNode(INCR, 	$2); 		 if($2->info == STRING){ yyerror("Cannot increment other element other than integer or reals."); 																 $$->info = INTEGER;}}
				| DECR lvalue							{$$ = uniNode(DECR, 	$2); 		 if($2->info == STRING){ yyerror("Cannot decrement other element other than integer or reals."); 																 $$->info = INTEGER;}}
				| '&' lvalue %prec ADDR   {$$ = uniNode(PTR,    $2);     if($2->info == NUMBER){ yyerror("Cannot address real numbers");													 																		   $$->info = INTEGER;}}
				| '-' expres %prec UMINUS {$$ = uniNode(UMINUS, $2); 		 if($2->info == STRING){ yyerror("Cannot make negative other element than integer or reals");																		 $$->info = INTEGER;}}
				| '!' expres							{$$ = uniNode(FACTR,  $2); 		 if($2->info == STRING){ yyerror("Cannot calculate factorial other element other than integer or reals."); 											 $$->info = INTEGER;}}
				| expres '!'							{$$ = uniNode(FACTL,  $1); 		 if($1->info == STRING){ yyerror("Cannot calculate factorial other element other than integer or reals."); 											 $$->info = INTEGER;}}
				| '~' expres							{$$ = uniNode(NOT,    $2); 		 if($2->info == STRING){ yyerror("Cannot symmetrical number of strings."); 																										 $$->info = NUMBER;}}
				| expres '/' expres				{$$ = binNode(DIV,  	$1, $3); if($1->info == STRING || $3->info == STRING) yyerror("Cannot divide two elements other than integers or reals."); 							 $$->info = INTEGER;}
				| expres '%' expres				{$$ = binNode(MOD,  	$1, $3); if($1->info == STRING || $3->info == STRING) yyerror("Cannot calculate modular of two elements other than integers or reals."); $$->info = INTEGER;}
				| expres '+' expres				{$$ = binNode(PLUS, 	$1, $3); if($1->info == STRING || $3->info == STRING) yyerror("Cannot sum two elements other than integers or reals.");									 $$->info = INTEGER;}
				| expres '-' expres				{$$ = binNode(SUBX,   $1, $3); if($1->info == STRING || $3->info == STRING) yyerror("Cannot subtract two elements other than integers or reals."); 						 $$->info = INTEGER;}
				| expres '*' expres				{$$ = binNode(NUL,    $1, $3); if($1->info == STRING || $3->info == STRING) yyerror("Cannot multiply two elements other than integers or reals.");             $$->info = INTEGER;}
				| expres '<' expres				{$$ = binNode(LT,   	$1, $3);}
				| expres '>' expres				{$$ = binNode(GT,   	$1, $3);}
				| expres GE  expres				{$$ = binNode(GE,   	$1, $3);}
				| expres LE  expres				{$$ = binNode(LE,   	$1, $3);}
				| expres '=' expres				{$$ = binNode(EQ,     $1, $3);}
				| expres NE  expres				{$$ = binNode(NE,     $1, $3);}
				| expres '&' expres				{$$ = binNode(AND,    $1, $3);}
				| expres '|' expres				{$$ = binNode(OR,     $1, $3);}
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
