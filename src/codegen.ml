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

(** The LLVM global module *)
let llmodule = L.create_module llcontext "microc"

(** Some useful LLVM IR type to use in the code generation *)
let int_type      = L.i32_type llcontext
let float_type    = L.float_type llcontext
let bool_type     = L.i1_type llcontext
let char_type     = L.i8_type llcontext
let void_type     = L.void_type llcontext
let array_type    = L.array_type 
let pointer_type  = L.pointer_type 

(** Additional function that declares in the current module the print prototype *)  
let print_decl =
  let print_t = L.function_type void_type [| int_type |] in
  L.declare_function "print" print_t llmodule

(** Additional function that declares in the current module the getint prototype *)  
let get_decl =
  let getint_t = L.function_type int_type [| |] in
  L.declare_function "getint" getint_t llmodule

(** Additional function that converts Ast types into LLVM types
  @param ty The AST type to convert
  @return lltype
*)
let rec convert_type (ty: typ) =
  match ty with
  | TypI                -> int_type
  | TypF                -> float_type
  | TypB                -> bool_type
  | TypC                -> char_type
  | TypV                -> void_type
  | TypA (t, i)         -> pointer_type (convert_type t)  
  | TypP (t)            -> pointer_type (convert_type t)
  | TypFun (t, id, lst) -> convert_type t

(** 
  Additional function that initializes the variables
  @param ty The AST type of the variable to initialize
  @return llvalue
*)
let rec initialize_var (ty: typ) =
  match ty with
  | TypI                -> L.const_int int_type 0
  | TypF                -> L.const_float float_type 0.0
  | TypB                -> L.const_int bool_type 0
  | TypC                -> L.const_int char_type 0
  | TypA (t, i)         -> (
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
  | TypP (t)            -> L.const_pointer_null (convert_type t)
  | _                   -> failwith Error_msg.codegen_vardec_err

(** A table mapping a binary operator in the LLVM 
  function that implemets it and its name 
*)
let primitive_operators = 
  [
    (int_type, Add),      (L.build_add, "add") 
  ; (float_type, Add),    (L.build_fadd, "add") 
  ; (int_type, Mult),     (L.build_mul, "mul")
  ; (float_type, Mult),   (L.build_fmul, "mul")
  ; (int_type, Sub),      (L.build_sub, "sub")
  ; (float_type, Sub),    (L.build_fsub, "sub")
  ; (int_type, Div),      (L.build_sdiv, "div")
  ; (float_type, Div),    (L.build_fdiv, "div")
  ; (int_type, Mod),      (L.build_srem, "mod")
  ; (float_type, Mod),    (L.build_frem, "mod")

  ; (int_type, Equal),    (L.build_icmp L.Icmp.Eq, "equal") 
  ; (float_type, Equal),  (L.build_fcmp L.Fcmp.Oeq, "equal") 
  ; (int_type, Neq),      (L.build_icmp L.Icmp.Ne, "neq")
  ; (float_type, Neq),    (L.build_fcmp L.Fcmp.One, "neq")
  ; (int_type, Less),     (L.build_icmp L.Icmp.Slt, "less")
  ; (float_type, Less),   (L.build_fcmp L.Fcmp.Olt, "less")
  ; (int_type, Leq),      (L.build_icmp L.Icmp.Sle, "leq")
  ; (float_type, Leq),    (L.build_fcmp L.Fcmp.Ole, "leq")
  ; (int_type, Greater),  (L.build_icmp L.Icmp.Sgt, "greater")
  ; (float_type, Greater),(L.build_fcmp L.Fcmp.Ogt, "greater")
  ; (int_type, Geq),      (L.build_icmp L.Icmp.Sge, "geq")
  ; (float_type, Geq),    (L.build_fcmp L.Fcmp.Oge, "geq")

  ; (bool_type, And),     (L.build_and, "and")
  ; (bool_type, Or),      (L.build_or, "or")
  ]
    
(** Additional function that adds the parameter of a function
    inside its environment
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

(** Additional function that adds a terminator to the builder
  @param builder
  @param f
*)
let termination_stmt builder f = 
  match L.block_terminator (L.insertion_block builder) with
  | Some(i)   -> ()
  | None      -> ignore(f builder)

(** This will be used for the code generation for the expressions
  @param env the environment of llvalues
  @param builder
  @param ex The expression
  @return llvalue of the expression
*)
let rec codegen_expr ex env builder = 

  (** Additional function that manages the variables as pointers,
      arrays or simple ones.
    @param acc The AST access to define
    @param env The environment of llvalues
    @return llvalue of the access
  *)
  let rec codegen_access acc env = 
    match acc.node with
    | AccVar (id)       -> (
        try Symbol_table.lookup id env
        with NotFoundEntry -> failwith Error_msg.name_err
    )
    | AccDeref (e)      -> (
        let le = codegen_expr e env builder in
        L.build_gep le [| L.const_int int_type 0 |] "ptr" builder
    )   
    | AccIndex (a, e)   -> (
        let le = codegen_expr e env builder in
        let la = codegen_access a env in
        if(L.classify_type (L.element_type (L.type_of la)) = Array)
        then L.build_gep la [| L.const_int int_type 0 ; le |] "idx" builder
        else (
          let idx = L.build_load la "idx" builder in
          L.build_gep idx [| le |] "idx" builder 
        )
    )
  in
  match ex.node with
  | ILiteral (i)          -> L.const_int int_type i
  | FLiteral (f)          -> L.const_float float_type f
  | CLiteral (c)          -> L.const_int char_type (Char.code c)
  | BLiteral (b)          -> L.const_int bool_type (Bool.to_int b)
  | NLiteral ()           -> L.const_int void_type 0
  | Access (acc)          -> (
      let lvalue = codegen_access acc env in
      let ctype = L.classify_type (L.element_type (L.type_of lvalue)) in

      if (ctype = Array)
      then L.build_struct_gep lvalue 0 "acc" builder
      else L.build_load lvalue "acc" builder
  ) 
  | Assign (acc, e)       -> (
      let lvalue = codegen_access acc env in
      let le = codegen_expr e env builder in
      L.build_store le lvalue builder
  )
  | Addr (acc)            -> codegen_access acc env
  | UnaryOp (u, e)        -> (
      let lexpr = codegen_expr e env builder in
      match u with
      | Neg   -> L.build_neg lexpr "neg" builder 
      | Not   -> L.build_not lexpr "not" builder
      | Incr  -> (
        let (llvm_operator, label) = List.assoc (L.type_of lexpr, Add) primitive_operators in 
        llvm_operator lexpr (L.const_int int_type 1) label builder
      )
      | Decr  -> (
        let (llvm_operator, label) = List.assoc (L.type_of lexpr, Sub) primitive_operators in 
        llvm_operator lexpr (L.const_int int_type 1) label builder
      )
  )
  | BinaryOp (b, e1, e2)  -> (
      let le1 = codegen_expr e1 env builder in
      let le2 = codegen_expr e2 env builder in

      let (llvm_operator, label) = List.assoc (L.type_of le1, b) primitive_operators in 
      llvm_operator le1 le2 label builder
  )
  | Call (id, lst)        -> (
      match id with
      | "print"   -> (
        let lvalue = L.lookup_function id llmodule in
        match lvalue with
        | None    -> failwith Error_msg.name_err
        | Some(f) -> (
          let le = codegen_expr (List.hd (lst)) env builder in
          L.build_call f (Array.of_list ([le])) "" builder
        )
      )
      | "getint"  -> (
        let lvalue = L.lookup_function id llmodule in
        match lvalue with
        | None    -> failwith Error_msg.name_err
        | Some(f) -> L.build_call f (Array.of_list ([])) id builder
      )
      | _         -> (
        let rec gen_call lst env acc = 
        match lst with
        | []      -> acc
        | x::xs   -> (
            match x.node with
            | Assign (_, ex) -> (
                ignore (codegen_expr x env builder);
                let lexpr = codegen_expr ex env builder in
                gen_call xs env (acc @ [lexpr])
            )
            | _               -> (
                let lexpr = codegen_expr x env builder in
                gen_call xs env (acc @ [lexpr])
            )
        )
        in
        let lvalue =
          try Symbol_table.lookup id env 
          with NotFoundEntry -> failwith Error_msg.name_err
        in
        let vlst = gen_call lst env [] in
        L.build_call lvalue (Array.of_list (vlst)) "" builder
      )
  )

(** This function is responsable for the code generation of
    the statements
  @param fdec The function definition
  @param env The environment of llvalues
  @param builder
  @param st The statement to generate
  @return builder
*)
let rec codegen_stmt fdec env builder st =
  match st.node with
  | If (ex, s1, s2)         -> (
      let bcont = L.append_block llcontext "cont" fdec in
      let bthen = L.append_block llcontext "then" fdec in
      let belse = L.append_block llcontext "else" fdec in
  
      let bt = codegen_stmt fdec (Symbol_table.begin_block env) (L.builder_at_end llcontext bthen) s1 in
      let bf = codegen_stmt fdec (Symbol_table.begin_block env) (L.builder_at_end llcontext belse) s2 in 
      termination_stmt bt (L.build_br bcont) |> ignore;
      termination_stmt bf (L.build_br bcont) |> ignore;

      let code_ex = codegen_expr ex env builder in
      L.build_cond_br code_ex bthen belse builder |> ignore;

      L.builder_at_end llcontext bcont
  )
  | While (ex, s)           -> (
      let bguard = L.append_block llcontext "guard" fdec in 
      let bbody  = L.append_block llcontext "wbody" fdec in
      L.build_br bguard builder |> ignore;
      
      let bt = codegen_stmt fdec (Symbol_table.begin_block env) (L.builder_at_end llcontext bbody) s in
      termination_stmt bt (L.build_br bguard) |> ignore;

      let bcont  = L.append_block llcontext "cont" fdec in
      let code_ex = codegen_expr ex env (L.builder_at_end llcontext bguard) in
      L.build_cond_br code_ex bbody bcont (L.builder_at_end llcontext bguard) |> ignore;
      
      L.builder_at_end llcontext bcont
  )
  | Expr (e)                -> let _ = codegen_expr e env builder in builder
  | Return (e)              -> (
      match e with
      | Some (e1)  -> (
          let code_ex = codegen_expr e1 env builder in
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
            match ty with
            | TypA (t, i) -> (
                let size =  
                  match i with
                  | Some(i) -> i
                  | None    -> 1
                in
                let array_init = Array.make size (initialize_var t) in
                let lvalue = L.const_array (convert_type t) array_init in
                let local = L.build_alloca (array_type(convert_type t) size) id builder in
                let env = Symbol_table.add_entry id local env in
                let loc = L.build_gep local [| L.const_int int_type 0 |] "idx" builder in
                L.build_store lvalue loc builder |> ignore;
                
                check_block xs env builder
            )
            | TypP (tp)   -> (
                let lvalue = L.const_pointer_null (convert_type ty) in
                let local = L.build_alloca (convert_type ty) id builder in
                let env = Symbol_table.add_entry id local env in
                L.build_store lvalue local builder |> ignore;

                check_block xs env builder
            )
            | _           -> (
                let lvalue = initialize_var ty in
                let local = L.build_alloca (convert_type ty) id builder in
                let env = Symbol_table.add_entry id local env in
                L.build_store lvalue local builder |> ignore;

                check_block xs env builder
            )
          )
          | Stmt (s)        -> (
            let b = codegen_stmt fdec env builder s in
            check_block xs env b
          )
        )
      in
      check_block lst env builder
  )

(** This function is responsable for the code generation 
    of the top declarations
  @param env the environment of llvalues
  @param topdecls The list of top declarations
  @return llmodule
*)
let rec codegen_topdec env topdecls =
  match topdecls with
  | []      -> llmodule
  | x::xs   -> (
    match x.node with
    | Fundecl (fdec)    -> (
        (* function declaration *)
        let lst_par = List.map (fun (a,b) -> a) fdec.formals in
        let par_types = Array.of_list (List.map (convert_type) lst_par) in 
        let ftype = L.function_type (convert_type fdec.typ) par_types in
        let dec = L.define_function fdec.fname ftype llmodule in
        let env = Symbol_table.add_entry fdec.fname dec env in
  
        (* create bb *)
        let builder = L.builder_at_end llcontext (L.entry_block dec) in
        let env' = 
          add_params (Symbol_table.begin_block env) fdec.formals (Array.to_list (L.params dec)) builder 
        in
        let builder' = codegen_stmt dec env' builder fdec.body in
        
        (* return value *)
        let fret = 
          match fdec.typ with
            | TypV  -> L.build_ret_void
            | _     -> L.build_ret (initialize_var fdec.typ) 
        in
        termination_stmt builder' fret |> ignore;
        codegen_topdec env xs
    )
    | Vardec (ty, id)   -> (
        match ty with
        | TypP (_)    -> (
            let ptr = L.const_pointer_null (convert_type ty) in
            let value = L.define_global id ptr llmodule in
            let env = Symbol_table.add_entry id value env in
            codegen_topdec env xs
        )
        | _           -> (
            let value =  L.define_global id (initialize_var ty) llmodule in
            let env = Symbol_table.add_entry id value env in
            codegen_topdec env xs
        )
    )
  )

(** Entry point for the code generation, it inserts the two
    library functions' definition inside the environment and
    then it starts the generation from the top declarations
  @param Prog(topdecls) the AST program
  @return llmodule the entire generated module
*)
let to_ir (Prog(topdecls)) =
  print_decl |> ignore;
  get_decl |> ignore;

  let env = Symbol_table.empty_table in
    codegen_topdec env topdecls;