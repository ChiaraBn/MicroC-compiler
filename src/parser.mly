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
  |  EOF                      {Prog([])}
;
