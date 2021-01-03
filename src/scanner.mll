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
            ("int", INT);
            ("char", CHAR);
            ("bool", BOOL);
            ("void", VOID);
            ("NULL", NULL);
            
            ("true", TRUE);
            ("false", FALSE)
        ]
}

let letter = ['a'-'z' 'A'-'Z']
let lit_letter = '\'' letter+ '\''
let digit = ['0' - '9']
let identifier = _ | letter (letter | digit | '_')*

rule token = parse
    | digit+ as inum        { 
                              let num = int_of_string inum in
			                  LINT(num)
                            }
    | lit_letter+ as ichar  { 
                              let c = ichar in
                              LCHAR(c)
                            }
    
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
    
    | '<'                   { LESS }
    | '>'                   { GREATER }
    | "<="                  { LEQ }
    | ">="                  { GEQ }
    | "=="                  { EQ }
    | "!="                  { NOTEQ }
    | '='                   { ASSIGN }
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
                              Printf.printf "comments (%d) end\n";
                              Lexing.new_line lexbuf; token lexbuf;
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