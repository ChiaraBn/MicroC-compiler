(**
  The module that implements the overall Code Generation 
  for the MicroC language.
*)

open Ast
open Symbol_table
open Error_msg

(** A shorthand for referring to Llvm module *)
module L = Llvm

(** The LLVM global context *)
let llcontext = L.global_context()

(** Some useful LLVM IR type to use in the code generation *)
let int_type      = L.i32_type llcontext
let float_type    = L.float_type llcontext
let bool_type     = L.i1_type llcontext
let char_type     = L.i8_type llcontext
let void_type     = L.void_type llcontext
let array_type    = L.array_type 
let pointer_type  = L.pointer_type 

(** Additional function that declares in the current module the print prototype 
  @param llvm_module
*)  
let print_decl llvm_module =
  let print_t = L.function_type void_type [| int_type |] in
  L.declare_function "print" print_t llvm_module

(** Additional function that declares in the current module the getint prototype
  @param llvm_module
*)  
let get_decl llvm_module =
  let getint_t = L.function_type int_type [| |] in
  L.declare_function "getint" getint_t llvm_module

(** Additional function that converts Ast types into LLVM types
  @return lltype
*)
let rec convert_type (ty: typ) =
  match ty with
  | TypI            -> int_type
  | TypF            -> float_type
  | TypB            -> bool_type
  | TypC            -> char_type
  | TypV            -> void_type
  | TypA (t, i)     -> pointer_type (convert_type t)  
  | TypP (t)        -> pointer_type (convert_type t)
  | TypFun (lst, t) -> convert_type t

(** 
  @return llvalue
*)
let rec initialize_var ty =
  match ty with
  | TypI            -> L.const_int int_type 0
  | TypF            -> L.const_float float_type 0.0
  | TypB            -> L.const_int bool_type 0
  | TypC            -> L.const_int char_type 0
  | TypA (t, i)     -> (
      let lltype = convert_type t in
      let llvalue = initialize_var t in
      let size =  
        match i with
        | Some(i) -> i
        | None    -> 1
      in
      let array_init = Array.make size llvalue in
      L.const_array lltype array_init
   )
  | TypP (t)        -> L.const_pointer_null (convert_type t)
  | _               -> Util.raise_semantic_error dummy_pos Error_msg.codegen_vardec_err

(** 
  @param env the environment of llvalues
  @param params the list of parameters
  @param llparams the llvalue of the parameters
  @param builder
*)
let add_params env params llparams builder = 
  let add_formal e (t, name) lvalue = L.set_value_name name lvalue;

  let single_p = L.build_alloca (convert_type t) name builder in
  L.build_store lvalue single_p builder |> ignore;
  Symbol_table.add_entry name single_p e in
  List.fold_left2 add_formal env params llparams

(** 
  @param builder
  @param f
*)
let termination_stmt builder f = 
  match L.block_terminator (L.insertion_block builder) with
  | Some(i)   -> ()
  | None      -> ignore(f builder)

(** 
  @return llvalue
*)
let build_op fi ff lv1 lv2 label builder = 
  let t = L.type_of lv1 in
  match t with 
  | int_type    -> fi lv1 lv2 label builder
  | float_type  -> ff lv1 lv2 label builder

(** 
  @param ex
  @param env the environment of llvalues
  @param builder
  @return llvalue of the expression
*)
let rec codegen_expr ex env builder = 

  (**
    @return llvalue of the access
  *)
  let rec codegen_access acc env = 
    match acc.node with
    | AccVar (id)         -> (
      let lvalue = 
        try Symbol_table.lookup id env
        with NotFoundEntry -> Util.raise_semantic_error acc.loc Error_msg.name_err
      in L.build_load lvalue id builder
    )
    | AccDeref (e)        -> codegen_expr e env builder        
    | AccIndex (a, e)     -> codegen_access a env

  in
  match ex.node with
  | ILiteral (i)          -> L.const_int int_type i
  | FLiteral (f)          -> L.const_float float_type f
  | CLiteral (c)          -> L.const_int char_type (Char.code c)
  | BLiteral (b)          -> L.const_int bool_type (Bool.to_int b)
  | NLiteral ()           -> L.const_int void_type 0
  | Access (acc)          -> codegen_access acc env
  | Assign (acc, e)       -> (
      let lvalue = codegen_access acc env in
      let lexpr = codegen_expr e env builder in
      L.build_store lexpr lvalue builder
  )
  | Addr (acc)            -> codegen_access acc env
  | UnaryOp (u, e)        -> (
      let lexpr = codegen_expr e env builder in
      match u with
      | Neg   -> L.build_neg lexpr "neg" builder 
      | Not   -> L.build_not lexpr "not" builder
      | Incr 
      | Decr  -> (
          let t = L.type_of lexpr in
          match t with
          | int_type   -> (
              if (u == Incr) 
              then L.build_add lexpr (L.const_int int_type 1) "incr" builder
              else L.build_sub lexpr (L.const_int int_type 1) "decr" builder
          )
          | float_type -> (
              if (u == Incr) 
              then L.build_sub lexpr (L.const_float float_type 1.0) "decr" builder
              else L.build_fsub lexpr (L.const_float float_type 1.0) "decr" builder
          )
      )
  )
  | BinaryOp (b, e1, e2)  -> (
      let le1 = codegen_expr e1 env builder in
      let le2 = codegen_expr e2 env builder in
      match b with
      | Add       -> build_op (L.build_add) (L.build_fadd) le1 le2 "add" builder
      | Sub       -> build_op (L.build_sub) (L.build_fsub) le1 le2 "sub" builder
      | Mult      -> build_op (L.build_mul) (L.build_fmul) le1 le2 "mul" builder
      | Div       -> build_op (L.build_sdiv) (L.build_fdiv) le1 le2 "div" builder
      | Mod       -> build_op (L.build_srem) (L.build_frem) le1 le2 "mod" builder

      | Equal     -> build_op (L.build_icmp L.Icmp.Eq)  (L.build_fcmp L.Fcmp.Oeq) le1 le2 "equal" builder
      | Neq       -> build_op (L.build_icmp L.Icmp.Ne)  (L.build_fcmp L.Fcmp.One) le1 le2 "neq" builder
      | Less      -> build_op (L.build_icmp L.Icmp.Slt) (L.build_fcmp L.Fcmp.Olt) le1 le2 "less" builder
      | Leq       -> build_op (L.build_icmp L.Icmp.Sle) (L.build_fcmp L.Fcmp.Ole) le1 le2 "leq" builder
      | Greater   -> build_op (L.build_icmp L.Icmp.Sgt) (L.build_fcmp L.Fcmp.Ogt) le1 le2 "greater" builder
      | Geq       -> build_op (L.build_icmp L.Icmp.Sge) (L.build_fcmp L.Fcmp.Oge) le1 le2 "geq" builder
      
      | And       -> L.build_and le1 le2 "and" builder
      | Or        -> L.build_or  le1 le2 "or" builder

      | _         -> Util.raise_semantic_error ex.loc Error_msg.unknown_op_err
  )
  | Call (id, lst)        -> L.const_int int_type 0

(** 
  @param fdec
  @param env the environment of llvalues
  @param builder
  @param st
  @return builder
*)
let rec codegen_stmt fdec env builder st =
  match st.node with
  | If (ex, s1, s2)         -> (
      let bthen = L.append_block llcontext "then" fdec in
      let belse = L.append_block llcontext "else" fdec in
      let bcont = L.append_block llcontext "cont" fdec in
  
      let bt = codegen_stmt fdec (Symbol_table.begin_block env) (L.builder_at_end llcontext bthen) s1 in
      let bf = codegen_stmt fdec (Symbol_table.begin_block env) (L.builder_at_end llcontext belse) s2 in 
      termination_stmt bt (L.build_br bcont) |> ignore;
      termination_stmt bf (L.build_br bcont) |> ignore;

      let code_ex = codegen_expr ex env builder in
      L.build_cond_br code_ex bthen belse builder |> ignore;

      L.builder_at_end llcontext bcont
  )
  | While (ex, s)           -> (
      let bguard = L.append_block llcontext "while" fdec in
      let bbody  = L.append_block llcontext "wbody" fdec in
      let bcont  = L.append_block llcontext "cont" fdec in

      L.build_br bguard builder |> ignore;

      let bt = codegen_stmt fdec (Symbol_table.begin_block env) (L.builder_at_end llcontext bbody) s in
      termination_stmt bt (L.build_br bguard) |> ignore;

      let code_ex = codegen_expr ex env builder in
      L.build_cond_br code_ex bbody bcont (L.builder_at_end llcontext bguard) |> ignore;
      
      L.builder_at_end llcontext bcont
  )
  | Expr (e)                -> let _ = codegen_expr e env builder in builder
  | Return (e)              -> (
      match e with
      | Some (e)  -> (
          let code_ex = codegen_expr e env builder in
          termination_stmt builder (L.build_ret (code_ex)) |> ignore;
          builder
      )
      | None      ->  (
        termination_stmt builder (L.build_ret_void) |> ignore;
        builder
      )
  )
  | Block (lst)             -> (
      let rec check_block lst env builder =
        match lst with
        | []      -> builder
        | x::xs   -> (
          match x.node with
          | Dec (ty, id)    -> (
            let lvalue = initialize_var ty in
            let _ = L.set_value_name id lvalue in
            let local = L.build_alloca (convert_type ty) id builder in
            let env = Symbol_table.add_entry id local env in
            L.build_store lvalue local builder |> ignore;
            check_block xs env builder
          )
          | Stmt (s)        -> (
            let b = codegen_stmt fdec env builder s in
            check_block xs env b
          )
        )
      in
      check_block lst env builder
  )

(** 
  @param llvm_module
  @param env the environment of llvalues
  @param topdecls
  @return llmodule
*)
let rec codegen_topdec llvm_module env topdecls =
  match topdecls with
  | []      -> llvm_module
  | x::xs   -> (
    match x.node with
    | Fundecl (fdec)    -> (
        (* function declaration *)
        let lst_par = List.map (fun (a,b) -> a) fdec.formals in
        let par_types = Array.of_list (List.map (convert_type) lst_par) in 
        let ftype = L.function_type (convert_type fdec.typ) par_types in
        let dec = L.define_function fdec.fname ftype llvm_module in
        let env = Symbol_table.add_entry fdec.fname dec env in
  
        (* create bb *)
        let builder = L.builder_at_end llcontext (L.entry_block dec) in
        let env' = 
          add_params (Symbol_table.begin_block env) fdec.formals (Array.to_list (L.params dec)) builder 
        in
        let _ = codegen_stmt dec env' builder fdec.body in
        
        (* return value *)
        let fret = 
          match fdec.typ with
            | TypV  -> L.build_ret_void
            | _     -> L.build_ret (L.const_int int_type 0) 
        in
        termination_stmt builder fret |> ignore;
        codegen_topdec llvm_module env xs
    )
    | Vardec (ty, id)   -> (
        let value =  L.define_global id (initialize_var ty) llvm_module in
        Symbol_table.add_entry id value env |> ignore;
        codegen_topdec llvm_module env xs
    )
  )

(** 
  @return llmodule the entire generated module
*)
let to_ir (Prog(topdecls)) =
  let llmodule = L.create_module llcontext "microc" in
  print_decl llmodule |> ignore;
  get_decl llmodule |> ignore;

  let env = Symbol_table.empty_table in
    codegen_topdec llmodule env topdecls;