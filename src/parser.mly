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
%token IF ELSE RETURN WHILE FOR 
%token INT CHAR BOOL VOID /*   */
/* %token TRUE FALSE vedi con lbool*/
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV  "/"
%token MODULE "%" 
%token ASSIGN "="
%token EQ "==" 
%token NOTEQ "!="
%token LESS "<" 
%token GREATER ">"
%token LEQ "<=" 
%token GEQ ">=" 
%token NOT "!"
%token LPAREN "("
%token RPAREN ")"
%token LSQUAREPAREN "[" 
%token RSQUAREPAREN "]" 
%token LBRACHPAREN "{" 
%token RBRACHPAREN "}" 
%token COMMA ","
%token SEMICOLON ";"
%token RIF "&" 
%token AND_BIT "&&"
%token OR_BIT "||"
%token EOF

%token <string> ID
%token <char> LCHAR
%token <int> LINT
%token <bool> LBOOL
%token <unit> NULL

/* Precedence and associativity specification */
%right    ASSIGN
%left     OR_BIT
%left     AND_BIT
%left     EQ NOTEQ
%nonassoc GREATER LESS GEQ LEQ
%left     PLUS MINUS 
%left     TIMES DIV MODULE
%nonassoc NOT RIF
%nonassoc LSQUAREPAREN 

/* Starting symbol */
%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */

%%

/* Grammar specification */
program:
  | f = list(topdecl)        
    { Prog f }
  | EOF
    { Prog ([]) }
  | error                     
    { raise (Syntax_error "parser error") }
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
  | "{"; cont = separated_list (SEMICOLON, stmt); "}"
    { (Block [ (Stmt(List.hd(cont)) |@| $loc) ] |@| $loc) } 
 
  /*
  | "{"; cont = declarations; "}"
    { Block (cont) |@| $loc }
  

  | "{"; cont = separated_list(SEMICOLON, var); "}"
    { (Block [ ( Dec(List.hd(fst cont), List.hd(snd cont)) |@| $loc) ]) |@| $loc } 
  ;
  

declarations:
  | { [] }
  | v = var; vv = declarations
    { (Dec(fst v, snd v)|@| $loc); v::vv }
;
*/

stmt:
  | RETURN; e = expr; ";" 
    { (Return (Some(e)) |@| $loc) }

  | e = expr; ";" 
    { (Expr (e) |@| $loc) }

  | b = block 
    { b }

  | WHILE; "("; e = expr; ")"; b = block 
    { (While (e, b) |@| $loc) }

  | FOR; "("; e1 = expr; ";"; e2 = expr; ";"; e3 = expr; ")"; b = block
    { (For (e1, e2, e3, b) |@| $loc) } 

  | IF; "("; e = expr; ")"; s1 = stmt; ELSE; s2 = stmt  
    { (If (e, s1, s2) |@| $loc ) }

  | IF; "("; e = expr; ")"; s1 = stmt
    { (IfThen (e, s1) |@| $loc ) }

;

expr: 
  | r = rexpr 
    { r }

  | l = lexpr    
    { (Access (l) |@| $loc) }
;

lexpr:
  | id = ID 
    { (AccVar(id) |@| $loc ) }

  | "("; l = lexpr; ")"
    { l } 

  | "*"; l = lexpr 
    { l }

  | "*"; a = aexpr 
    { (AccDeref(a) |@| $loc ) }

  | l = lexpr; "["; e = expr; "]"
    { (AccIndex(l, e) |@| $loc ) }
;

rexpr: 
  | a = aexpr 
    { a }

  | id = ID; "("; cont = separated_list(COMMA, expr); ")" 
    { Call(id, cont) |@| $loc }

  | l = lexpr; "="; e = expr 
    { Assign(l, e) |@| $loc }

  | "!"; e = expr 
    { UnaryOp(Not, e) |@| $loc }

  | "-"; e = expr 
    { UnaryOp(Neg, e) |@| $loc }

  | e1 = expr; b = binop; e2 = expr 
    { BinaryOp(b, e1, e2) |@| $loc }
;

aexpr:
  | i = LINT 
    { ILiteral(i) |@| $loc } 

  | c = LCHAR 
    { CLiteral(c) |@| $loc }
  
  | b = LBOOL 
    { BLiteral (b) |@| $loc }
  
  | n = NULL
    { NLiteral() |@| $loc }

  | "("; r = rexpr; ")" 
    { r }

  | "&"; l = lexpr
    { Addr(l) |@| $loc }
;

binop:
  | "+" 
    { Add }
  | "-" 
    { Sub }
  | "*" 
    { Mult }
  | "%" 
    { Mod }
  | "/" 
    { Div }
  | "&&" 
    { And_bit }
  | "||" 
    { Or_bit }
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
;