
(** The error message produced if the main function signature is wrong. *)
let main_err = "Main function can only be of int or void type"

(** The error message produced if the main function is not defined. *)
let no_main_err = "Main function missing"

(** The error message produced if a variable is unbound. *)
let unbound_var_err = "Unbound variable"

(** The error message produced if a variable is void type *)
let variable_void_err = "Variables of type void are not allowed"

(** The error message produced if there's a mismatch in the type definitions *)
let type_err = "Type not defined"

(** The error message produced if a component has already been declared *)
let element_decl_err = "Element already defined"

(** The error message produced if the return value 
    of a function is not correct.
 *)
let return_fun_err = "Function cannot return Arrays or Pointers"

(** The error message produced if there's a mismatch in the operators. *)
let arith_op_err = "Arithmetic operators expect only integer values"

(** The error message produced if there's a mismatch in the operators. *)
let logic_op_err = "Logical operators expect only boolean values"

(** The error message produced if the guard
    of an [if] or [while] does not have type [bool]. *)
let guard_err = "Guards must have type bool"

(** The error message produced if there's an assignment between arrays*)
let array_assign_err = "Array cannot be assigned"