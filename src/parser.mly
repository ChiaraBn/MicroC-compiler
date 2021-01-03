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
%token INT CHAR BOOL VOID NULL 
%token TRUE FALSE
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
%token EOF
%token AND_BIT OR_BIT
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
    { raise (Syntax_error "error in program") }
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

topdecl:
  | v = var; ";"
    { Vardec(fst v, snd v) |@| $loc  }
  /*
  | f = fundecl
    {  { loc=$loc; node=f; id=0} } 
  */
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

 | t = types; id = ID; "["; a = INT; "]"
    { (TypA (t, Some(int)), id) } 
;


/*

fundecl: 
  | t = typ; id = ID; "("; cont = separated_list(COMMA, vardecl); v = vardecl; ")"; b = block 
    { Prog (Fundecl({ typ=t; fname=id; formals=cont; body=b })) |@| $loc }
;

block: 
  | "{"; cont = separated_list (SEMICOLON, stmt); "}"
      { Prog (cont) } 
  | "{"; cont = separated_list(SEMICOLON, vardecl); "}"
    { Prog  ( [] ) }
;
*/

/*
stmt:
  | "return"; e = expr; ";" 
    { Prog (Access(e)) }

  | e = expr; ";" 

  | b = block 

  | WHILE; "("; e = expr; ")"; b = block 

  | FOR; "("; e1 = expr; ";"; e2 = expr; ";"; e3 = expr; ")"; b = block

  | IF; "("; e = expr; ")"; s1 = stmt; ELSE; s2 = stmt  

  | IF; "("; e = expr; ")"; s1 = stmt
;

expr: 
  | r = expr 
    { Prog r }

  | l = lexpr    
    { Prog l }
;

lexpr:
  | id = ID 
    { Prog (AccVar(id)) }

  | "("; l = lexpr; ")" 

  | "*"; l = lexpr 

  | "*"; a = AExpr 

  | l = lexpr; "["; e = expr; "]"
;

rexpr: 
  | a = Aexpr 

  | id = ID; "("; cont = separated_list(COMMA, expr); e2 = expr; ")" 

  | l = lexpr; "="; e = expr 

  | "!"; e = expr 

  | "-"; e = expr 

  | e1 = expr; b = binop; e2 = expr 
;

binop:
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
;

aexpr:
  | INT 

  | CHAR 

  | BOOL 

  | "NULL" 

  | "("; r = rexpr; ")" 

  | "&"; l = lexpr
;
*/