/*
* MicroC Parser specification
*/

%{
    open Ast
    open Util
    open Printf

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
%token AND "&&" OR "||"
%token EOF

%token <string> ID
%token <char>   LCHAR
%token <int>    LINT
%token <float>  LFLOAT
%token <bool>   LBOOL
%token <unit>   NULL

/* Precedence and associativity specification */
%nonassoc NO_ELSE
%nonassoc ELSE
%right    ASSIGN
%left     OR
%left     AND
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
  
  | t = types; id = ID "("; cont = separated_list(COMMA, var); ")"; b = block 
    { Fundecl ({ typ=t; fname=id; formals=cont; body=b }) |@| $loc }
;

types: 
  | INT
    { TypI }
  | FLOAT
    { TypF }
  | CHAR 
    { TypC }
  | BOOL
    { TypB }
  | VOID
    { TypV }
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

%inline block:
  | "{"; c = list(cont); "}"
    { (Block(c) |@| $loc) }
;

cont:
  | s = stmt
    { (Stmt(s) |@| $loc) } 

  | d = var; ";"
    { (Dec(fst d, snd d) |@| $loc) } 
;

stmt:
  | s = selection_stmt
    { s }
  
  | l = loop_stmt
    { l }

  | b = block
    { b }

  | RETURN; e = option(expr); ";"
    { (Return (e) |@| $loc) }

  | e = expr; ";"
    { (Expr (e) |@| $loc) }
;

selection_stmt:
  | IF; "("; e = expr; ")"; s1 = stmt %prec NO_ELSE
    { (If (e, s1, (Block [] |@| $loc)) |@| $loc ) }

  | IF; "("; e = expr; ")"; s1 = stmt; ELSE; s2 = stmt
    { (If (e, s1, s2) |@| $loc ) }
;

loop_stmt:
  | WHILE; "("; e = expr; ")"; b = stmt
    { (While (e, b) |@| $loc) }

  | DO; "{"; b = stmt; "}"; WHILE; "("; e = expr; ")" 
    { (While (e, b) |@| $loc) }

  | FOR; "("; e1 = expr; ";"; e2 = expr; ";"; e3 = expr; ")"; b = stmt
    { (For (e1, e2, e3, b) |@| $loc) }
  
  | FOR; "("; ";"; e = expr; ";"; ")"; b = stmt
    { (While (e, b) |@| $loc) }
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

  | INCR; e = expr
    { 
      let e2 = ( (BinaryOp (Add, (Access ((AccDeref(e)) |@| $loc) |@| $loc), ((ILiteral 1) |@| $loc))) |@| $loc )
      in ( Assign ((AccDeref(e) |@| $loc), e2) |@| $loc ) 
    }
  
  | e = expr; INCR
    { 
      let e2 = ( (UnaryOp (Incr, (Access ((AccDeref(e)) |@| $loc) |@| $loc))) |@| $loc )
      in ( Assign ((AccDeref(e) |@| $loc), e2) |@| $loc ) 
    }

  | DECR; e = expr
    { 
      let e2 = ( (BinaryOp(Sub, (Access ((AccDeref(e)) |@| $loc) |@| $loc), ((ILiteral 1) |@| $loc))) |@| $loc )
      in (Assign((AccDeref(e) |@| $loc), e2) |@| $loc) 
    }
  
  | e = expr; DECR
    { 
      let e2 = ( (UnaryOp (Decr, (Access ((AccDeref(e)) |@| $loc) |@| $loc))) |@| $loc )
      in ( Assign ((AccDeref(e) |@| $loc), e2) |@| $loc ) 
    }
;

binary:
  | e1 = expr; b = op; e2 = expr
    { BinaryOp(b, e1, e2) |@| $loc }
;

%inline op:
  | "+"
    { Add }
  | "-"
    { Sub }
  | "*"
    { Mult }
  | "/"
    { Div }
  | "%"
    { Mod }

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
    { And }
  | "||" 
    { Or }
;