{
    open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let identifier = _ | letter (letter | digit | '_')*

rule token = parse
    | digit+                { LINT }
    | letter+               { LCHAR }
    | identifier            { ID }
    
    | "if"                  { IF }
    | "else"                { ELSE }
    | "return"              { RETURN }
    | "while"               { WHILE }
    | "for"                 { FOR }
    | "int"                 { INT }
    | "char"                { CHAR }
    | "void"                { VOID }
    | "NULL"                { NULL }
    | "bool"                { BOOL }

    | "true"                { TRUE }
    | "false"               { FALSE }
    
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIV }
    | '%'                   { MODULE }
    | "=="                  { EQ }
    | '='                   { ASSIGN }
    | "!="                  { NOTEQ }
    | '!'                   { NOT }
    | '<'                   { LESS }
    | '>'                   { GREATER }
    | "<="                  { LEQ }
    | ">="                  { GEQ }

    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | '['                   { LSQUAREPAREN }
    | ']'                   { RSQUAREPAREN }
    | '{'                   { LBRACHPAREN }
    | '}'                   { RBRACHPAREN }
    | "&&"                  { AND_BIT }
    | '&'                   { AND }
    | "||"                  { OR_BIT }
    | ';'                   { SEMICOLON }

    | "/*"                  { print_endline "comments start"; comments 0 lexbuf }
    | "//"                  { print_endline "comments start"; comments_one_line lexbuf }

    | [' ' '\t']            { next_token lexbuf }
    | '\n'                  { Lexing.new_line lexbuf; next_token lexbuf }
    | eof                   { EOF }

    | _ as c                { Util.raise_lexer_error lexbuf ("Illegal character " ^ Char.escaped c) }

and comments level = parse
    | "*/"                  {
                              Printf.printf "comments (%d) end\n" level;
                              if level = 0 then token lexbuf
                              else comments (level-1) lexbuf
                            }

    | "/*"                  { 
                              Printf.printf "comments (%d) start\n" (level+1); 
                              comments (level+1) lexbuf 
                            }
    | _                     { comments level lexbuf }
    | eof                   { print_endline "comments are not closed"; raise End_of_file }

and comments_one_line = parse
    | _                     { comments_one_line lexbuf }
    | '\n'                  {                                
                              Printf.printf "comments (%d) end\n";
                              Lexing.new_line lexbuf; next_token lexbuf;
                            }

{
    let () =
    let lexbuf = Lexing.from_channel stdin in
    try
        while true do
        token lexbuf
        done
    with End_of_file -> ()
}