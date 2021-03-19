(**
  controllo semantico
 *)

open Ast
open Symbol_table
open Error_msg

type context = typ Symbol_table.t

(** TODO cancella *)
let debug env = Symbol_table.print_elems env


(** Additional function that inserts new values into the symbol table
  @param name string for the value
  @param ty the type
  @param env context so far
  @param loc the location for the value
  @param msg the error message in case of a duplicate
  @return the new environment
 *)
let add_value (name: identifier) (ty: typ) (env: context) (loc: position) (msg: string) =
  try Symbol_table.add_entry name ty env
  with Symbol_table.DuplicateEntry -> (
    Util.raise_semantic_error loc msg;
  )

(** Inserting the library functions into the symbol table 
  @return the initial symbol table
 *)
let initialize =
  let t0 = Symbol_table.empty_table in
  let t1 = add_value "print" (TypFun([TypI],TypV)) t0 dummy_pos Error_msg.type_err in
  add_value "getint" (TypFun([],TypI)) t1 dummy_pos Error_msg.type_err

(** *)
let rec custom_fold f acc y =
  match y with
  | [] -> (acc, [])
  | x::xs -> let (acc', y) = f acc x in
              let (res, ys) = custom_fold f acc' xs in
              (res, y::ys)

(** *)
let check_void ty x =
  match ty with 
  | TypV    -> Util.raise_semantic_error x.loc Error_msg.variable_void_err
  | _       -> ty

(** *)
let check_stmt (env: context) (st: stmt) =
  match st.node with 
  | _ -> (st)
  (** | If of expr * stmt * stmt        
  | While of expr * stmt             
  | For of expr * expr * expr * stmt 
  | Expr of expr                     
  | Return of expr option            
  | Block 
  *)

(** *)
let check_topdecl (env: context) (td: topdecl) =
    match td.node with
    | Fundecl (fdec)    -> (
                            let ty = fdec.typ in 
                              match ty with 
                              | TypA (t,i)  -> Util.raise_semantic_error td.loc Error_msg.return_fun_err
                              | TypP (t)    -> Util.raise_semantic_error td.loc Error_msg.return_fun_err
                              | _           -> (

                                let env' = 
                                  add_value fdec.fname ty env td.loc Error_msg.element_decl_err;
                                in 
                                  let nblock = Symbol_table.begin_block env' in
                                  let g = 
                                    let rec add e lst =
                                      match lst with
                                      | []        -> e
                                      | (t,i)::xs -> add (add_value i (check_void t td) e td.loc Error_msg.element_decl_err) xs
                                    in add nblock fdec.formals
                                  in
                                    let fbody = check_stmt g fdec.body in
                                    (env', {loc = td.loc; node = Fundecl ({typ = ty; 
                                                                          fname = fdec.fname; 
                                                                          formals = fdec.formals; 
                                                                          body = fbody
                                                                }); id = td.id })
                              )
                          )

    (** TODO: controlla per gli array e la memoria *)
    | Vardec (ty, id)   -> ( let t = check_void ty td 
                             in 
                              let env' = add_value id ty env td.loc Error_msg.element_decl_err;
                              in (env', {loc = td.loc; node = Vardec(t, id); id = td.id })
                          )

(** *)
let check_main (env: context) = 
  debug env;
  let main = try Symbol_table.lookup "main" env 
             with Symbol_table.NotFoundEntry -> Util.raise_semantic_error dummy_pos Error_msg.no_main_err
  in
    match main with 
      | TypI  -> ()
      | TypV  -> ()
      | _     -> Util.raise_semantic_error dummy_pos Error_msg.main_err

(** 
  Starting point for the program's type checking
  @param topdecls. Program's AST
 *)
let check (Prog(topdecls)) =  
  let initial_env = initialize in
    let (final_env, topd) = 
      custom_fold (fun x y -> (check_topdecl x y)) initial_env topdecls in check_main final_env;
  
  Prog (topdecls)