(**
  The module that implements the overall Code Generation 
  for the MicroC language.
*)

open Ast
open Symbol_table
open Error_msg
open Semant

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
  @return block
*)
let codegen_stmt fdec env builder stmt =
  match stmt.node with
  | If (ex, s1, s2)         -> (
      let bcont = L.append_block llcontext "cont" fdec in 
      L.builder_at_end llcontext bcont
  )
  | _       -> (
      let bcont = L.append_block llcontext "cont" fdec in 
      L.builder_at_end llcontext bcont
  )

  (*
  | If of expr * stmt * stmt         (** Conditional                    *)
  | While of expr * stmt             (** While loop                     *)
  | For of expr * expr * expr * stmt (** For loop                       *)
  | Expr of expr                     (** Expression statement   e;      *)
  | Return of expr option            (** Return statement               *)
  | Block of stmtordec list          (** Block: grouping and scope      *)
  *)

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
            let term = L.block_terminator (L.insertion_block builder) in 
            match term with
              | Some(i) -> codegen_topdec llvm_module env xs
              | None    -> (
                fret builder |> ignore;
                codegen_topdec llvm_module env xs
              )
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