/*
* MicroC Parser specification
*/

%{
    open Ast

    (* Define here your utility functions *)

%}

/* Tokens declarations */
%token IF ELSE RETURN WHILE FOR 
%token INT CHAR BOOL VOID NULL
%token TRUE FALSE
%token PLUS MINUS TIMES DIV MODULE ASSIGN
%token EQ NOTEQ LESS GREATER LEQ GEQ NOT
%token LPAREN RPAREN LSQUAREPAREN RSQUAREPAREN LBRACHPAREN RBRACHPAREN EOF
%token AND_BIT AND OR_BIT SEMICOLON
%token <string> ID LCHAR
%token <int> LINT

/* Precedence and associativity specification */
%right    ASSIGN
%left     OR_BIT
%left     AND_BIT
%left     EQ NOTEQ
%nonassoc GREATER LESS GEQ LEQ
%left     PLUS MINUS 
%left     TIMES DIV MODULE
%nonassoc NOT AND
%nonassoc LSQUAREPAREN 

/* Starting symbol */
%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */
program:
  | expr
  | EOF                      
    { Prog([]) }
;

expr:
  | e1 = simple_expr
    { e1 }
  /*
  | e1 = expr PLUS e2 = expr
    { AccBinOp(Add, e1, e2) }
  | e1 = expr MINUS e2 = expr
    { AccBinOp(Sub, e1, e2) }
  | e1 = expr TIMES e2 = expr
    { AccBinOp(Mult, e1, e2) }
  | e1 = expr DIV e2 = expr
    { AccBinOp(Div, e1, e2) }
  | e1 = expr MODULE e2 = expr
    { AccBinOp(Mod, e1, e2) }

  | e1 = expr LESS e2 = expr
    { AccBinOp(Less, e1, e2) }
  | e1 = expr GREATER e2 = expr
    { AccBinOp(Greater, e1, e2) }
  | e1 = expr LEQ e2 = expr
    { AccBinOp(Leq, e1, e2) }
  | e1 = expr GEQ e2 = expr
    { AccBinOp(Geq, e1, e2) }
  | e1 = expr EQ e2 = expr
    { AccBinOp(Equal, e1, e2) }
  | e1 = expr NOTEQ e2 = expr
    { AccBinOp(Neq, e1, e2) }
  | e1 = expr ASSIGN e2 = expr
    { AccBinOp(Assign, e1, e2) }
  | NOT e1 = expr
    { AccUnOp(Not, e1) }
    */
;

simple_expr:
  | i = LINT
    { AccInt(i) }
  | c = LCHAR
    { AccChar (c) }
  | TRUE
    { AccBool (true) }
  | FALSE
    { AccBool (false) }
  | id = ID
    { AccVar(id) }
  | LPAREN e = expr RPAREN
    { e }
;

/*
application:
  | e1 = simple_expr e2=simple_expr
    { Call(e1, e2) }
  | e1 = application e2=simple_expr
    { Call(e1, e2) }
;
*/