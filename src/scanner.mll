{
    open Parser

    exception Lexing_error of string

    let create_hashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
        tbl

    let keyword_table =
        create_hashtable 8 [
            ("if", IF);
            ("else", ELSE);
            ("return", RETURN);

            ("while", WHILE);
            ("for", FOR);
            ("do", DO);

            ("int", INT);
            ("float", FLOAT);
            ("char", CHAR);
            ("bool", BOOL);
            ("void", VOID);
        ]
}

let letter = ['a'-'z' 'A'-'Z']
let lit_char = ['a'-'z' 'A'-'Z' '0'-'9']

let digit = ['-']?['0' - '9']
let fdigit = ['0'-'9']+'.'['0'-'9']+ (['E' 'e'] ['+' '-']? ['0'-'9']+)?
let identifier = _ | letter (letter | digit | '_')*

rule token = parse
    | digit+ as inum        { 
                              let num = int_of_string inum in
			                  LINT(num)
                            }
    | fdigit as fnum        { 
                              let num = float_of_string fnum in
                              LFLOAT(num) 
                            }

    | "true"                { LBOOL(true) }
    | "false"               { LBOOL(false) }

    | "null"                { NULL() }

    | lit_char as lchar     { LCHAR(lchar) }
    
    | identifier as word    { 
                              try
                               Hashtbl.find keyword_table word
                              with Not_found ->
			                   ID(word)
                            }

    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIV }
    | '%'                   { MODULE }

    | "++"                  { INCR }
    | "--"                  { DECR }
    | "+="                  { ASSIGN_PLUS }
    | "-="                  { ASSIGN_MINUS }
    | "*="                  { ASSIGN_TIMES }
    | "/="                  { ASSIGN_DIV }
    | "%="                  { ASSIGN_MODULE }

    | '<'                   { LESS }
    | '>'                   { GREATER }
    | "<="                  { LEQ }
    | ">="                  { GEQ }
    | '='                   { ASSIGN }
    | "=="                  { EQ }
    | "!="                  { NOTEQ }
    | '!'                   { NOT }
    
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | '['                   { LSQUAREPAREN }
    | ']'                   { RSQUAREPAREN }
    | '{'                   { LBRACHPAREN }
    | '}'                   { RBRACHPAREN }
    | "&&"                  { AND_BIT }
    | '&'                   { RIF }
    | "||"                  { OR_BIT }
    | ';'                   { SEMICOLON }
    | ','                   { COMMA }

    | "/*"                  { print_endline "comments start"; comments 0 lexbuf }
    | "//"                  { print_endline "comments start"; comments_one_line lexbuf }

    | [' ' '\t']            { token lexbuf }
    | '\n'                  { Lexing.new_line lexbuf; token lexbuf }
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
                              Printf.printf "comment end\n";
                              Lexing.new_line lexbuf; token lexbuf;
                            }