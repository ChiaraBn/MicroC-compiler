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
    with Symbol_table.DuplicateEntry -> 
      Util.raise_semantic_error loc msg

(** Additional function that searches for a value in the symbol table
  @param id string for the value
  @param env context so far
  @param loc the location for the value
  @param msg the error message in case the value is not in the table
  @return the found value
 *)
let check_lookup (id: identifier) (env: context) (loc: position) (msg: string) = 
  try Symbol_table.lookup id env
    with Symbol_table.NotFoundEntry ->
      Util.raise_semantic_error loc msg

(** Additional function that raises an error if the type of the symbol id void
  @param ty the input type
  @param x the symbol to check
  @return the type of the symbol
 *)
let check_void ty x =
  match ty with 
  | TypV    -> Util.raise_semantic_error x.loc Error_msg.variable_void_err
  | _       -> ty
  
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
  | []        -> (acc, [])
  | x::xs     -> let (acc', y) = f acc x in
                  let (res, ys) = custom_fold f acc' xs in
                  (res, y::ys)
                              
(**
  @param env the environment
  @param ex the expression to check
  @return (ty, e)
 *)
let rec type_expr (env: context) (ex: expr) =
  
  let rec type_access (acc: access) (env: context) = 
    match acc.node with
    | AccVar (id)         -> 
      check_lookup id env acc.loc Error_msg.unbound_var_err
    | AccDeref (e)        -> type_expr env e
    | AccIndex (a, e)     -> type_access a env
  in

  match ex.node with
  | ILiteral (i)          -> TypI
  | FLiteral (f)          -> TypF
  | CLiteral (c)          -> TypC
  | BLiteral (b)          -> TypB
  | NLiteral ()           -> TypV

  | Access (a)            -> type_access a env
  | Assign (a, e)         -> type_access a env
  | Addr (a)              -> type_access a env

  | UnaryOp (u, e)        -> type_expr env e
  | BinaryOp (b, e1, e2)  -> (
      match b with
      | Add | Sub | Mult | Div  
      | Mod         -> (
        let t1 = type_expr env e1 in 
          let t2 = type_expr env e2 in 
          if (t1 != t2)
            then Util.raise_semantic_error ex.loc Error_msg.coercion_err
          else t1;
        )
      | Equal | Neq | Less | Leq | Greater | Geq | And
      | Or       -> (
        let t1 = type_expr env e1 in 
          let t2 = type_expr env e2 in 
          if (t1 != t2)
            then Util.raise_semantic_error ex.loc Error_msg.coercion_err
          else TypB;
        )
      | _           -> Util.raise_semantic_error ex.loc Error_msg.unknown_op_err
    )
  | Call (id, lst)        -> check_lookup id env ex.loc Error_msg.name_err

(** *)
let rec check_expr (env: context) (e: expr) =
  match e.node with 
  | Access (acc)          -> (
      let ty = check_void (type_expr env e) e in 
        match ty with
        | TypA (ta, i)    -> if (ta != TypI) (** TODO  *)
                              then Util.raise_semantic_error e.loc Error_msg.index_err
                              else { loc = e.loc; node = Access(acc); id = e.id }
        
        | _               -> { loc = e.loc; node = Access(acc); id = e.id }
    )
  | Assign (acc, ex)      -> (
      let ta = type_expr env e in
        let te = type_expr env ex in
          if (ta == te && te == TypA(te, Some(0)))
          then Util.raise_semantic_error ex.loc Error_msg.array_assign_err
          else ( if (ta != te)
            then Util.raise_semantic_error ex.loc Error_msg.coercion_err
            else ({ loc = e.loc; node = Assign(acc, ex); id = e.id });
          )
    )
  | Addr (acc)            -> (
      let ta = type_expr env e in
        match ta with 
        | TypP (t)    -> { loc = e.loc; node = Addr(acc); id = e.id }
        | _           -> Util.raise_semantic_error e.loc Error_msg.pointer_err
    )

  | ILiteral (i)          -> { loc = e.loc; node = ILiteral(i); id = e.id }
  | FLiteral (f)          -> { loc = e.loc; node = FLiteral(f); id = e.id }
  | CLiteral (c)          -> { loc = e.loc; node = CLiteral(c); id = e.id }
  | BLiteral (b)          -> { loc = e.loc; node = BLiteral(b); id = e.id }
  | NLiteral ()           -> { loc = e.loc; node = NLiteral(); id = e.id }

  | UnaryOp (u, e1)        -> (
      match u with
      | Not       -> (
          let t = type_expr env e1 in
          if (t != TypB)
            then Util.raise_semantic_error e.loc Error_msg.logic_op_err
            else { loc = e.loc; node = UnaryOp(u, e1); id = e.id };
        )
      | Incr
      | Decr
      | Neg       -> (
          let t = type_expr env e1 in
            if (t == TypI || t == TypF)
              then { loc = e.loc; node = UnaryOp(u, e1); id = e.id }
              else Util.raise_semantic_error e.loc Error_msg.arith_op_err;
        )
    )
  | BinaryOp (b, e1, e2)  -> (
      match b with
      | And 
      | Or          -> (
          let t1 = type_expr env e1 in
            let t2 = type_expr env e2 in
            if (t1 != t2)
              then Util.raise_semantic_error e.loc Error_msg.coercion_err
              else if (t1 == TypB)
                then { loc = e.loc; node = BinaryOp(b, e1, e2); id = e.id }
                else Util.raise_semantic_error e.loc Error_msg.logic_op_err;
        )
      | Equal
      | Neq        -> (
          let t1 = type_expr env e1 in
            let t2 = type_expr env e2 in
            if (t1 != t2)
              then Util.raise_semantic_error e.loc Error_msg.coercion_err
              else if (t1 == TypB || t1 == TypF || t1 == TypI)
                then { loc = e.loc; node = BinaryOp(b, e1, e2); id = e.id }
                else Util.raise_semantic_error e.loc Error_msg.logic_op_err;
        )
      | Add | Sub | Mult | Div | Mod | Less | Leq | Greater
      | Geq        -> (
          let t1 = type_expr env e1 in
            let t2 = type_expr env e2 in
            if (t1 != t2)
              then Util.raise_semantic_error e.loc Error_msg.coercion_err
              else if (t1 == TypI || t1 == TypF)
                then { loc = e.loc; node = BinaryOp(b, e1, e2); id = e.id }
                else Util.raise_semantic_error e.loc Error_msg.arith_op_err;
        )

      | _           -> Util.raise_semantic_error e.loc Error_msg.unknown_op_err
    )
  | Call (id, lst)        -> (
      let tf = check_lookup id env e.loc Error_msg.name_err in
        let rec check_list (lst: expr list) (env: context) (acc: expr list) = 
          match lst with
          | []      -> []
          | x::xs   -> let dex = check_expr env x in
                        check_list xs env (acc @ [dex])
        in
          if (tf != TypP(tf))
          then (
             let clist = check_list lst env [] in
              { loc = e.loc; node = Call (id, clist); id = e.id }
          )
          else Util.raise_semantic_error e.loc Error_msg.return_fun_err;         
    )

(**
  @param env the environment
  @param st the statement to check
  @return the statement checked
  *)
let rec check_stmt (env: context) (st: stmt) =
  match st.node with 
  | If (ex, s1, s2)       -> (          (** TODO *)
    let ty = type_expr env ex in 
      match ty with 
      | TypB    -> (
                  let c1 = check_stmt (Symbol_table.begin_block env) s1 in
                    let c2 = check_stmt (Symbol_table.begin_block env) s2 in
                    let ce = check_expr env ex in
                    { loc = st.loc; node = If(ce, c1, c2); id = st.id }
                  )

      | _       -> Util.raise_semantic_error st.loc Error_msg.guard_err
    )
  | While (ex, s)         -> (
    let ty = type_expr env ex in
      match ty with
        | TypB    -> let c1 = check_stmt (Symbol_table.begin_block env) s in
                      { loc = st.loc; node = While(ex, c1); id = st.id }
        
        | _       -> Util.raise_semantic_error st.loc Error_msg.guard_err
    )
  | For (e1, e2, e3, s)   -> (
    let c1 = check_stmt (Symbol_table.begin_block env) s in
        let ex1 = check_expr env e1 in 
        let te2 = type_expr env e2 in
          match te2 with
          | TypB      -> ( 
              let ex2 = check_expr env e2 in  
                let ex3 = check_expr env e2 in
            { loc = st.loc; node = For(ex1, ex2, ex3, c1); id = st.id }
            )
          | _         -> Util.raise_semantic_error st.loc Error_msg.guard_err
    )
  | Expr (e)              -> (
    let e1 = check_expr env e in 
      { loc = st.loc; node = Expr(e1); id = st.id }
    )
  | Return (e)            -> (
    match e with
      | None       -> { loc = st.loc; node = Return(None); id = st.id }
      | Some (e1)  -> let ce1 = check_expr env e1 in 
                    { loc = st.loc; node = Return(Some(ce1)); id = st.id }
    )
  | Block (lstd)          -> (
    let rec check_list (lst: stmtordec list) (env: context) (acc: stmtordec list) = 
      match lst with
      | []      -> []
      | x::xs   -> (
          match x.node with
          | Dec (ty, id)   -> (
              let t = check_void ty x in
              let env' = add_value id t env x.loc Error_msg.element_decl_err in
                let dec = { loc = st.loc; node = Dec(t, id); id = x.id } in 
                  check_list xs env' (acc @ [dec])
            )

          | Stmt (s)       -> ( 
              let test = check_stmt env s in
                let s' = { loc = st.loc; node = (Stmt test); id = x.id } in
                  check_list xs env (acc @ [s'])
            )
        )
    in
      let clst = check_list lstd (Symbol_table.begin_block env) [] in
      { loc = st.loc; node = Block(clst); id = st.id }
  )

(** 
  @param env the environment
  @param td the declaration to check
  @return (env, td), the changed environment and the declaration
 *)
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
                | []          -> e
                | (t,i)::xs   -> 
                    add (add_value i (check_void t td) e td.loc Error_msg.element_decl_err) xs
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
    | Vardec (ty, id)   -> ( 
      let t = check_void ty td in 
        let env' = add_value id ty env td.loc Error_msg.element_decl_err;
        in (env', {loc = td.loc; node = Vardec(t, id); id = td.id })
      )

(**
  @param env the environment
 *)
let check_main (env: context) = 
  debug env;
  let main = check_lookup "main" env dummy_pos Error_msg.no_main_err
  in
    match main with 
      | TypI  -> ()
      | TypV  -> ()
      | _     -> Util.raise_semantic_error dummy_pos Error_msg.main_err

(** 
  Starting point for the program's type checking
  @param topdecls the set of declaration that forms the program
 *)
let check (Prog(topdecls)) =  
  let initial_env = initialize in
    let (final_env, topd) = 
      custom_fold (fun x y -> (check_topdecl x y)) initial_env topdecls in check_main final_env;