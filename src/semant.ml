(**
  The module that implements the overall semantic and
  typing rules of the MicroC language.
*)

open Ast
open Symbol_table
open Error_msg

type context = typ Symbol_table.t


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

(** Additional function that raises an error if the type of the symbol is void
  @param ty the input type
  @param x the symbol to check
  @return the type of the symbol
*)
let check_void ty x =
  match ty with 
  | TypV    -> Util.raise_semantic_error x.loc Error_msg.variable_void_err
  | _       -> ty
  
(** Additional function that checkes whenever two types coincide.
  If they are equal, it returns the element (ret) passes as an argument,
  otherwise it raises an error.
  @param t1 the type of the first element
  @param t2 the type of the second element
  @param e the expression to be checked
  @param ret the element to return
*)
let check_type (t1: typ) (t2: typ) (e: expr) ret = 
  if (t1 == t2)
  then ret
  else ( 
    match t1 with
      | TypA (ta1, i)   -> 
        begin
        match t2 with
          | TypA (ta2, i)   -> Util.raise_semantic_error e.loc Error_msg.array_assign_err
          | _               -> Util.raise_semantic_error e.loc Error_msg.coercion_err
        end
      | _               -> Util.raise_semantic_error e.loc Error_msg.coercion_err
  )

(** Additional function that inserts the library functions into the symbol table 
  @return the initial symbol table
*)
let initialize =
  let t0 = Symbol_table.empty_table in
  let fprint = TypFun(TypV, "print", [TypI]) in
  let t1 = add_value "print" fprint t0 dummy_pos Error_msg.type_err in
  let fgetint = TypFun(TypI, "getint", []) in
    add_value "getint" fgetint t1 dummy_pos Error_msg.type_err

(** Additional function that implements a fold left map
  @param f the function to be applied
  @param acc the accolumator for the result of the function
  @param y the list of elements which the function should be applied
*)
let rec custom_fold f acc y =
  match y with
  | []        -> (acc, [])
  | x::xs     -> let (acc', y) = f acc x in
                  let (res, ys) = custom_fold f acc' xs in
                  (res, y::ys)

(** Additional function that creates a node access
  @param acc the expression of type access
*)
let make_access (acc: access) =
  match acc.node with
  | AccVar (id)         -> { loc = acc.loc; node = AccVar(id); id = acc.id }
  | AccDeref (e)        -> { loc = acc.loc; node = AccDeref(e); id = acc.id }
  | AccIndex (a, e)     -> { loc = acc.loc; node = AccIndex(a,e); id = acc.id }

(** Additional function that returns the type of the expression passed as parameter
  @param env the environment
  @param ex the expression to be checked
  @return (typ)
*)
let rec type_expr (env: context) (ex: expr) =

  (** Function that returns the type of a node access
    @param acc the expression to be checked
    @param env the environment
    @return (typ)
  *)
  let rec type_access (acc: access) (env: context) = 
    match acc.node with
    | AccVar (id)         -> (
        let t = check_lookup id env acc.loc Error_msg.unbound_var_err in
          match t with
          | TypP (tp)        -> tp
          | _                -> t
    )
    | AccDeref (e)        -> (
        match e.node with
        | Access(a) -> type_access a env
        | _         -> Util.raise_semantic_error acc.loc Error_msg.pointer_err
    ) 
    | AccIndex (a, e)     -> (
      let ta = type_access a env in 
      match ta with
      | TypA (t, i) -> (
        let te = type_expr env e in
        begin
          match te with 
          | TypI        -> t
          | _           -> Util.raise_semantic_error a.loc Error_msg.index_err
        end
        )
      | _           -> Util.raise_semantic_error a.loc Error_msg.array_index_err
    )
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
      | Mod      -> (
          let t1 = type_expr env e1 in 
          let t2 = type_expr env e2 in 
            check_type t1 t2 ex t1
        )
      | Equal | Neq | Less | Leq | Greater | Geq | And
      | Or       -> (
          let t1 = type_expr env e1 in 
          let t2 = type_expr env e2 in 
            check_type t1 t2 ex TypB
        )
      | _        -> Util.raise_semantic_error ex.loc Error_msg.unknown_op_err
  )
  | Call (id, lst)        -> (
      let f = check_lookup id env ex.loc Error_msg.name_err in
      match f with
      | TypFun (t, id, lst) -> t
      | _                   -> Util.raise_semantic_error ex.loc Error_msg.no_fun_err
  )

(** This function is used in order to check the semantic of an expression
  @param env the environment
  @param e the expression to be checked
  @return (e) the expression checked
*)
let rec check_expr (env: context) (e: expr) =
  match e.node with 
  | Access (acc)          -> (
      let _ta = type_expr env e in
      let ma = make_access acc in
      { loc = e.loc; node = Access(ma); id = e.id }
  )
  | Assign (acc, ex)      -> (
      let ta = type_expr env e in
      let te = type_expr env ex in
      let ma = make_access acc in
      let ret = { loc = e.loc; node = Assign(ma, ex); id = e.id } in
      check_type ta te e ret
  )
  | Addr (acc)            -> (
      let _ta = type_expr env e in
      let ma = make_access acc in
      { loc = e.loc; node = Addr(ma); id = e.id }
  )
  | ILiteral (i)          -> { loc = e.loc; node = ILiteral(i); id = e.id }
  | FLiteral (f)          -> { loc = e.loc; node = FLiteral(f); id = e.id }
  | CLiteral (c)          -> { loc = e.loc; node = CLiteral(c); id = e.id }
  | BLiteral (b)          -> { loc = e.loc; node = BLiteral(b); id = e.id }
  | NLiteral ()           -> { loc = e.loc; node = NLiteral(); id = e.id }
  | UnaryOp (u, e1)       -> (
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

      | _          -> Util.raise_semantic_error e.loc Error_msg.unknown_op_err
  )
  | Call (id, lst)        -> (
      let rec check_list (lst: expr list) (tlst: typ list) 
                         (env: context) (acc: expr list) = 
        match lst with
        | []        -> acc
        | x::xs     -> (
            let dex = check_expr env x in
            let tex = type_expr env x in 

            match List.hd tlst with
            | TypA(ta, _)   -> (
                match tex with
                | TypA(te,_)    -> (
                    if (ta != te) 
                    then Util.raise_semantic_error e.loc Error_msg.formal_fun_err
                    else check_list xs (List.tl tlst) env (acc @ [dex])
                )
                | _               -> Util.raise_semantic_error e.loc Error_msg.formal_fun_err
              )
            | TypP(tp)      -> (
                if (tp != tex) 
                then Util.raise_semantic_error e.loc Error_msg.formal_fun_err
                else check_list xs (List.tl tlst) env (acc @ [dex])
            )
            | _             -> (
                if (tex != List.hd tlst)
                then Util.raise_semantic_error e.loc Error_msg.formal_fun_err
                else check_list xs (List.tl tlst) env (acc @ [dex])
            )
        )
      in
      let f = check_lookup id env e.loc Error_msg.name_err in
      match f with
      | TypFun (t, id, tlst)  -> (
           if (List.length (lst) = List.length (tlst))
           then (
             let clist = check_list lst tlst env [] in
             { loc = e.loc; node = Call (id, clist); id = e.id }
           ) 
           else Util.raise_semantic_error e.loc Error_msg.formal_fun_err
      )
      | _                     -> Util.raise_semantic_error e.loc Error_msg.no_fun_err
  )

(** This function is used in order to check the semantic of a statement
  @param env the environment
  @param st the statement to check
  @param fdec the function declaration
  @return the statement checked
*)
let rec check_stmt (env: context) (st: stmt) fdec =
  match st.node with 
  | If (ex, s1, s2)       -> (
    let ty = type_expr env ex in 
      match ty with 
      | TypB    -> (
        let c1 = check_stmt (Symbol_table.begin_block env) s1 fdec in
        let c2 = check_stmt (Symbol_table.begin_block env) s2 fdec in
        let ce = check_expr env ex in
        { loc = st.loc; node = If(ce, c1, c2); id = st.id }
      )
      | _       -> Util.raise_semantic_error st.loc Error_msg.guard_err
  )
  | While (ex, s)         -> (
    let ty = type_expr env ex in
      match ty with
      | TypB    -> (
          let c1 = check_stmt (Symbol_table.begin_block env) s fdec in
          { loc = st.loc; node = While(ex, c1); id = st.id }
      )
      | _       -> Util.raise_semantic_error st.loc Error_msg.guard_err
    )
  | Expr (e)              -> (
    let e1 = check_expr env e in 
      { loc = st.loc; node = Expr(e1); id = st.id }
  )
  | Return (e)            -> (
    match e with
    | None       -> (
        if (fdec.typ == TypV)
        then { loc = st.loc; node = Return(None); id = st.id }
        else Util.raise_semantic_error st.loc Error_msg.return_val_err
    )
    | Some (e1)  -> (
        let ce1 = check_expr env e1 in 
        let te1 = type_expr env e1 in
        if (te1 != fdec.typ) 
        then Util.raise_semantic_error st.loc Error_msg.return_val_err
        else { loc = st.loc; node = Return(Some(ce1)); id = st.id }
    )
  )
  | Block (lstd)          -> (
    let rec check_list (lst: stmtordec list) (env: context) 
                       (acc: stmtordec list) (flag: bool) = 
      match lst with
      | []      -> acc
      | x::xs   -> (
          match x.node with
          | Dec (ty, id)   -> (
              let t = check_void ty x in
              let check_size t =
                match t with
                | TypA(ta, i) -> if (Option.get i > 0) then true else false
                | _           -> true
              in 
              if (check_size t) 
              then (
                let env' = add_value id t env x.loc Error_msg.element_decl_err in
                let dec = { loc = st.loc; node = Dec(t, id); id = x.id } in 
                
                if (flag)
                then check_list xs env' (acc) flag
                else check_list xs env' (acc @ [dec]) flag
              )
              else Util.raise_semantic_error st.loc Error_msg.size_array_err
            )
          | Stmt (s)       -> ( 
              let test = check_stmt env s fdec in
              let s' = { loc = st.loc; node = (Stmt test); id = x.id } in

              if (flag)
              then (
                Printf.fprintf stderr "Code unreachable \n";
                check_list xs env (acc) flag
              )
              else (
                match s.node with
                | Return (_)  -> check_list xs env (acc @ [s']) true
                | _           -> check_list xs env (acc @ [s']) flag
              )
            )
        )
    in
      let clst = check_list lstd (Symbol_table.begin_block env) [] false in
      { loc = st.loc; node = Block(clst); id = st.id }
  )

(** This function is used in order to check the semantic of a top declaration,
  which can be either a function or variable declaration
  @param env the environment
  @param td the declaration to check
  @return (env, td), the changed environment and the declaration
*)
let check_topdecl (env: context) (td: topdecl) =
  match td.node with
  | Fundecl (fdec)    -> (
      match fdec.typ with 
      | TypA (t,i)  -> Util.raise_semantic_error td.loc Error_msg.return_fun_err
      | TypP (t)    -> Util.raise_semantic_error td.loc Error_msg.return_fun_err
      | _           -> (
        let tf = TypFun (fdec.typ, fdec.fname, (List.map (fun (a,b) -> a) fdec.formals)) in
        let env' = add_value fdec.fname tf env td.loc Error_msg.element_decl_err in 
        let nblock = Symbol_table.begin_block env' in
        let g = 
          let rec add e lst =
            match lst with
            | []          -> e
            | (t,i)::xs   -> 
                add (add_value i (check_void t td) e td.loc Error_msg.element_decl_err) xs
          in add nblock fdec.formals
        in
        let fbody = check_stmt g fdec.body fdec in
        (env', {loc = td.loc; node = Fundecl ({typ = fdec.typ; 
                                              fname = fdec.fname; 
                                              formals = fdec.formals; 
                                              body = fbody}); id = td.id })
    )
  )
  | Vardec (ty, id)   -> ( 
      let t = check_void ty td in 
      let env' = add_value id ty env td.loc Error_msg.element_decl_err
      in (env', {loc = td.loc; node = Vardec(t, id); id = td.id })
    )

(** This function is used in order to check the semantic of the main function
  @param env the environment
*)
let check_main (env: context) = 
  let main = check_lookup "main" env dummy_pos Error_msg.no_main_err in
  match main with 
  | TypFun (t, id, lst) -> (
    if (List.length lst > 1) 
    then Util.raise_semantic_error dummy_pos Error_msg.main_param_err
    else (
      match t with
      | TypI 
      | TypV  -> ()
      | _     -> Util.raise_semantic_error dummy_pos Error_msg.main_err
    )
  )
  | _                   -> Util.raise_semantic_error dummy_pos Error_msg.no_fun_err
  
(** 
  Starting point for the program's type checking
  @param topdecls the set of declaration that forms the program
*)
let check (Prog(topdecls)) =  
  let initial_env = initialize in
  let (final_env, topd) = 
    custom_fold (fun x y -> (check_topdecl x y)) initial_env topdecls in 
    check_main final_env;
  Prog(topd)