/*
* MicroC Parser specification
*/

%{
    open Ast
    open Util

    exception SyntaxError of string

    (* Annotated node *)
    let (|@|) node loc = { node = node; loc = loc };;
%}

/* Tokens declarations */
%token IF ELSE RETURN WHILE FOR DO
%token INT FLOAT CHAR BOOL VOID 
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" MODULE "%" 
%token INCR "++" DECR "--" 

%token ASSIGN "="
%token ASSIGN_PLUS ASSIGN_MINUS ASSIGN_TIMES ASSIGN_DIV ASSIGN_MODULE 

%token EQ "==" NOTEQ "!="
%token LESS "<" GREATER ">" LEQ "<=" GEQ ">=" 
%token NOT "!"
%token LPAREN "(" RPAREN ")"
%token LSQUAREPAREN "[" RSQUAREPAREN "]" 
%token LBRACHPAREN "{" RBRACHPAREN "}" 
%token COMMA "," SEMICOLON ";"
%token RIF "&" 
%token AND_BIT "&&" OR_BIT "||"
%token EOF

%token <string> ID
%token <char>   LCHAR
%token <int>    LINT
%token <float>  LFLOAT
%token <bool>   LBOOL
%token <unit>   NULL

/* Precedence and associativity specification */
%right    ASSIGN              /* lowest priority */
%left     OR_BIT
%left     AND_BIT
%left     EQ NOTEQ
%nonassoc GREATER LESS GEQ LEQ
%left     PLUS MINUS INCR DECR 
%left     TIMES DIV MODULE
%right    UMINUS
%nonassoc NOT RIF
%nonassoc LSQUAREPAREN 

/* Starting symbol */
%start program
%type <Ast.program> program    

%%

/* Grammar specification */
program:
  | f = list(topdecl) EOF     
    { Prog f }

  | error                     
    { raise (Syntax_error "Parser error") }
;

topdecl:
  | v = var; ";"
    { Vardec (fst v, snd v) |@| $loc  }
  
  | v = var; "("; cont = separated_list(COMMA, var); ")"; b = block 
    { Fundecl ({ typ=fst v; fname=snd v; formals=cont; body=b }) |@| $loc }
;

types: 
  | INT
    { TypI }
  | CHAR 
    { TypC }
  | VOID
    { TypV }
  | BOOL
    { TypB }
  | FLOAT
    { TypF }
;

var:
  | t = types; id = ID
    { (t, id) }

  | t = types; "*"; id = ID
    { (TypP(t), id) }

  | t = types; "("; v = var; ")"
    { v }

  | t = types; id = ID; "["; "]"
    { (TypA (t, Some(0)), id) }

  | t = types; id = ID; "["; a = LINT; "]"
    { (TypA (t, Some(a)), id) } 
;

block:
  | "{"; c = separated_list(";", cont); "}"
    { (Block(c) |@| $loc) }
;

cont:
  | s = stmt
    { (Stmt(s) |@| $loc) } 

  | d = var
    { (Dec(fst d, snd d) |@| $loc) } 
;

stmt:
  | s = opened_stmt
    { s }
  | s = closed_stmt
    { s }
;

opened_stmt:
  | b = block 
    { b }

  | IF; "("; e = expr; ")"; s1 = simple_stmt
    { (IfThen (e, s1) |@| $loc ) }
  
  | IF; "("; e = expr; ")"; s1 = opened_stmt
    { (IfThen (e, s1) |@| $loc ) }

  | IF; "("; e = expr; ")"; s1 = closed_stmt; ELSE; s2 = opened_stmt
    { (If (e, s1, s2) |@| $loc ) }

  | WHILE; "("; e = expr; ")"; b = opened_stmt
    { (While (e, b) |@| $loc) }

  | DO; "{"; b = opened_stmt; "}"; WHILE; e = expr
    { (Do (b,e) |@| $loc) }

  | FOR; "("; e1 = expr; ";"; e2 = expr; ";"; e3 = expr; ")"; b = opened_stmt
    { (For (e1, e2, e3, b) |@| $loc) } 
; 

closed_stmt:
  | s = simple_stmt
    { s }

  | IF; "("; e = expr; ")"; s1 = closed_stmt; ELSE; s2 = closed_stmt
    { (If (e, s1, s2) |@| $loc ) }
  
  | WHILE; "("; e = expr; ")"; b = closed_stmt
    { (While (e, b) |@| $loc) }

  | DO; "{"; b = closed_stmt; "}"; WHILE; e = expr
    { (Do (b,e) |@| $loc) }

  | FOR; "("; e1 = expr; ";"; e2 = expr; ";"; e3 = expr; ")"; b = closed_stmt
    { (For (e1, e2, e3, b) |@| $loc) } 
;

simple_stmt:
  | RETURN; e = expr; ";" 
    { (Return (Some(e)) |@| $loc) }

  | e = expr; ";" 
    { (Expr (e) |@| $loc) }
;

expr:
  | r = rexpr
    { r }

  | l = lexpr
    { (Access (l) |@| $loc) }
;

rexpr:
  | p = primitive
    { p }

  | c = call
    { c }

  | a = assignment
    { a }

  | u = unary
    { u }

  | b = binary
    { b }
;

primitive:
  | i = LINT 
    { ILiteral(i) |@| $loc } 

  | f = LFLOAT 
    { FLiteral(f) |@| $loc } 

  | c = LCHAR 
    { CLiteral(c) |@| $loc }
  
  | b = LBOOL 
    { BLiteral (b) |@| $loc }

  | NULL
    { NLiteral() |@| $loc }

  | "("; r = rexpr; ")" 
    { r }

  | "&"; l = lexpr
    { Addr(l) |@| $loc }
;

lexpr:
  | id = ID 
    { (AccVar(id) |@| $loc ) }

  | "("; l = lexpr; ")"
    { l } 

  | "*"; l = lexpr 
    { l }

  | "*"; a = primitive 
    { (AccDeref(a) |@| $loc ) }

  | l = lexpr; "["; e = expr; "]"
    { (AccIndex(l, e) |@| $loc ) }
;

call:
  id = ID; "("; cont = separated_list(COMMA, expr); ")" 
    { Call(id, cont) |@| $loc }
;

assignment:
  | l = lexpr; ASSIGN; e = expr 
    { Assign(l, e) |@| $loc }

  | l = lexpr; ASSIGN_PLUS; e = expr %prec PLUS
    { Assign(l, BinaryOp(Add, (Access(l)|@| $loc), e)|@| $loc ) |@| $loc }
  
  | l = lexpr; ASSIGN_MINUS; e = expr %prec MINUS
    { Assign(l, BinaryOp(Sub, (Access(l)|@| $loc), e)|@| $loc ) |@| $loc }

  | l = lexpr; ASSIGN_TIMES; e = expr %prec TIMES
    { Assign(l, BinaryOp(Mult, (Access(l)|@| $loc), e)|@| $loc ) |@| $loc }
  
  | l = lexpr; ASSIGN_DIV; e = expr %prec DIV
    { Assign(l, BinaryOp(Div, (Access(l)|@| $loc), e)|@| $loc ) |@| $loc }
  
  | l = lexpr; ASSIGN_MODULE; e = expr %prec MODULE
    { Assign(l, BinaryOp(Mod, (Access(l)|@| $loc), e)|@| $loc ) |@| $loc }
;

unary:
  | "!"; e = expr 
    { UnaryOp(Not, e) |@| $loc }

  | "-"; e = expr %prec UMINUS
    { UnaryOp(Neg, e) |@| $loc }

  | "++"; e = expr
    { UnaryOp (Incr, e) |@| $loc }
  
  | e = expr; "++"
    { UnaryOp (Incr, e) |@| $loc }

  | "--"; e = expr
    { UnaryOp (Decr, e) |@| $loc }
  
  | e = expr; "--"
    { UnaryOp (Decr, e) |@| $loc }
;

binary:
  | e1 = expr; b = mult_op; e2 = expr %prec TIMES
    { BinaryOp(b, e1, e2) |@| $loc }
    
  | e1 = expr; b = add_op; e2 = expr %prec PLUS
    { BinaryOp(b, e1, e2) |@| $loc }

  | e1 = expr; b = boolean_op; e2 = expr %prec GREATER
    { BinaryOp(b, e1, e2) |@| $loc }
;

mult_op:
  | "*"
    { Mult }
  | "/"
    { Div }
  | "%"
    { Mod }
;

add_op:
  | "+"
    { Add }
  | "-"
    { Sub }
;

boolean_op:
  | "<" 
    { Less }
  | ">" 
    { Greater }
  | "<=" 
    { Leq }
  | ">=" 
    { Geq }
  | "==" 
    { Equal }
  | "!="
    { Neq }
  | "&&" 
    { And_bit }
  | "||" 
    { Or_bit }
;