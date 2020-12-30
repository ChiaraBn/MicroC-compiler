open Ast

let string_of_loc (startp, endp) =
  let start_column_number = startp.Lexing.pos_cnum  - startp.Lexing.pos_bol + 1 in
  let end_column_number = endp.Lexing.pos_cnum  - endp.Lexing.pos_bol + 1 in
  if start_column_number = end_column_number then
    string_of_int start_column_number
  else
    Printf.sprintf "%d-%d" start_column_number end_column_number

type 'v env = (string * 'v) list

(**
   Given an environment {env} and an identifier {x} it returns the data {x} is bound to.
   If there is no binding, it raises an exception.
 *)
let rec lookup env x =
    match env with
    | []        -> failwith (x ^ " not found")
    | (y, v)::r -> if x = y then v else lookup r x

(**
  Interpreter for expressions. 
  Given an expression {e} and an enviroment {env} that closes {e},
  this function evaluates {e} and returns the result of the computation.
 *)
let rec eval (e : expr) (env : typ env) : typ =
  match e.node with
    | ILiteral i -> TypI i
    | BLiteral b -> TypB (if b then 1 else 0)
    (* | Access x  -> lookup env x *)

    | BinaryOp(ope, e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      begin
      match (ope, v1, v2) with
      | (Mult, TypI i1, TypI i2) -> TypI (i1 * i2)
      | (Add, TypI i1, TypI i2) -> TypI (i1 + i2)
      | (Sub, TypI i1, TypI i2) -> TypI (i1 - i2)
      | (Div, TypI i1, TypI i2) -> TypI (if i2 != 0 then i1 / i2 else 0)
      | (Mod, TypI i1, TypI i2) -> TypI (i1 mod i2)

      | (Equal, TypI i1, TypI i2) -> TypI (if i1 = i2 then 1 else 0)
      | (Less, TypI i1, TypI i2) -> TypI (if i1 < i2 then 1 else 0)
      |  _ -> failwith "unknown primitive or wrong type"
      end
    (*
    | UnaryOp (ope, e1) ->
      let v1 = eval e1 in
      begin
      match (ope, v1) with
      | (Not, TypI i1) -> TypI (!i1)
      | (Neg, TypI i1) -> TypI (-i1)
      |  _ -> failwith "unknown primitive or wrong type"
      end
    *)

(* Entry point *)
let parse lexbuf =
(* failwith "Not implemented yet" *)

  try
    let prog = Parser.program Scanner.token lexbuf in
    (*
    Printf.printf "%s> %d\n%!" (string_of_loc prog.loc) (eval prog)
    *)
    match eval prog ([]) with
    | Int n -> Printf.printf "val - : %d\n" n
    | _     -> Printf.printf "val -: fun"

  with
    | Scanner.Lexing_error msg ->
        Printf.fprintf stderr "%s%!" msg
    | Parser.Error ->
        Printf.fprintf stderr "Syntax error.\n"