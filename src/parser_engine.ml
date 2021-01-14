(**
  Parser engine for the incremental API of Menhir.
 *)

open Ast
open Lexing

(* module I = UnitActionsParser.MenhirInterpreter *)

let parse lexbuf =
  try 
    Parser.program Scanner.token lexbuf 
  with
    | Util.Syntax_error msg -> Util.raise_syntax_error lexbuf msg
    | Parser.Error -> Util.raise_syntax_error lexbuf "Parser error"

(*
let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"

    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (parser.messages (I.number state)) with
        | Not_found -> "invalid syntax (no specific message for this eror)"


let rec parse lexbuf (checkpoint : Grammar.grammar I.checkpoint) =
  match checkpoint with

  | I.InputNeeded _env ->
      let token = scanner.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint

  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint

  | I.HandlingError _env ->
      let line, pos = Util.get_lexing_position lexbuf in
      let err = get_parse_error _env in
      raise (Util.Syntax_error (Some (line, pos), err))

  | I.Accepted v -> v
  
  | I.Rejected ->
      raise (Util.Syntax_error (None, "invalid syntax (parser rejected the input)"))
*)