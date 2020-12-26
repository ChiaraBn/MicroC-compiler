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
program :
    e = expr EOF { e }
;

expr:
  | e = simple_expr
    { e }

  | e1 = expr PLUS e2 = expr
    { BinaryOp(Add, e1, e2) }
  | e1 = expr MINUS e2 = expr
    { BinaryOp(Sub, e1, e2) }
  | e1 = expr TIMES e2 = expr
    { BinaryOp(Times, e1, e2) }
  | e1 = expr DIV e2 = expr
    { BinaryOp(Div, e1, e2) }
  | e1 = expr MODULE e2 = expr
    { BinaryOp(Mod, e1, e2) }

  | e1 = expr LESS e2 = expr
    { BinaryOp(Less, e1, e2) }
  | e1 = expr GREATER e2 = expr
    { BinaryOp(Greater, e1, e2) }
  | e1 = expr LEQ e2 = expr
    { BinaryOp(Leq, e1, e2) }
  | e1 = expr GEQ e2 = expr
    { BinaryOp(Geq, e1, e2) }
  | e1 = expr EQ e2 = expr
    { BinaryOp(Equal, e1, e2) }
  | e1 = expr NOTEQ e2 = expr
    { BinaryOp(Neg, e1, e2) }
  | e1 = expr ASSIGN e2 = expr
    { BinaryOp(Assign, e1, e2) }

  | e1 = NOT expr
    { UnaryOp(Not, e1) }
;

simple_expr:
  | i = LINT
    { ILiteral(i) }
  | c = LCHAR
    { CLiteral(c) }
  | TRUE
    { BLiteral(true) }
  | FALSE
    { BLiteral(false) }
  | id = ID
    { AccVar(id) }
  | LPAREN e = expr RPAREN
    { e }
;

application:
  | e1 = simple_expr e2=simple_expr
    { Call(e1, e2) }
  | e1 = application e2=simple_expr
    { Call(e1, e2) }
;

/* TODO è ambigua 
program : 
    | p = Topdecl* EOF
        { p }
    | EOF  
        { Prog([]) }

Topdecl :
    | Vardecl ";"  
    | Fundecl

Vardecl : 
    | Typ Vardesc

Vardesc : 
    | ID 
    | "*" Vardesc 
    | "(" Vardesc ")" 
    | Vardesc "[" "]" 
    | Vardesc "[" INT "]" 

Fundecl : 
    | Typ ID "("((Vardecl ",")* Vardecl)? ")" Block

Block :
    | "{" (Stmt 
    | Vardecl ";")* "}"

Typ : 
    | "int" 
    | "char" 
    | "void" 
    | "bool" 

Stmt :
    | "return" Expr ";" 
    | Expr ";" 
    | Block 
    | "while" "(" Expr ")" Block 
    | "for" "(" Expr? ";" Expr? ";" Expr? ")" Block
    | "if" "(" Expr ")" Stmt "else" Stmt  
    | "if" "(" Expr ")" Stmt

Expr : 
    | RExpr 
    | LExpr

LExpr : 
    | ID 
    | "(" LExpr ")" 
    | "*" LExpr 
    | "*" AExpr 
    | LExpr "[" Expr "]"

RExpr :
    | AExpr 
    | ID "(" ((Expr ",")* Expr)? ")" 
    | LExpr "=" Expr 
    | "!" Expr 
    |  "-" Expr 
    | Expr BinOp Expr 

BinOp : 
    | "+" 
    | "-" 
    | "*" 
    | "%" 
    | "/" 
    | "&&" 
    | "||" 
    | "<" 
    | ">" 
    | "<=" 
    | ">=" 
    | "==" 
    | "!="

 AExpr : 
    | INT 
    | CHAR 
    | BOOL 
    | "NULL" 
    | "(" RExpr ")" 
    | "&" LExpr
    ;
    */