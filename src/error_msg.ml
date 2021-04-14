
(** The error message produced if the main function signature is wrong *)
let main_err = "Main function can only be of int or void type"

(** The error message produced if the main function is not defined *)
let no_main_err = "Main function missing"

(** The error message produced if the main function has more than 1 parameter *)
let main_param_err = "Main function can not have more than 1 parameter"

(** The error message produced if the main function is not well defined *)
let no_fun_err = "It is not a function"

(** The error message produced if a variable is unbound *)
let unbound_var_err = "Unbound variable"

(** The error message produced if a variable is void type *)
let variable_void_err = "Variables of type void are not allowed"

(** The error message produced if there's a mismatch in the type definitions *)
let type_err = "Type not defined"

(** The error message produced if a symbol is not found *)
let name_err = "Name not found"

(** The error message produced if a component has already been declared *)
let element_decl_err = "Element already defined"

(** The error message produced if the return value 
    of a function is not correct
 *)
let return_fun_err = "Function cannot return Arrays or Pointers"

(** The error message produced if the return value 
    of a function does not match the definition
 *)
let return_val_err = "Mismatch in the return statement"

(** The error message produced if the return value is missing*)
let return_miss_err = "The return value is missing"

(** The error message produced if there's a mismatch
    in the parameters of a function
 *)
let formal_fun_err = "Parameters do not match in the function"

(** The error message produced if there's a mismatch in the operators *)
let arith_op_err = "Arithmetic operators expect only integer or float values"

(** The error message produced if there's a mismatch in the operators *)
let logic_op_err = "Logical operators expect only boolean values"

(** The error message produced if there's an unknown operator *)
let unknown_op_err = "The operator is not defined"

(** The error message produced if the guard
    of an [if] or [while] does not have type [bool] *)
let guard_err = "Guards must have type bool"

(** The error message produced if there's an assignment between arrays*)
let array_assign_err = "Array cannot be assigned"

(** The error message produced if the access via indexes is not done over an array *)
let array_index_err = "The operator is not an array"

(** The error message produced if an array has not size at least 1 *)
let size_array_err = "Array should have a size at least of 1"

(** The error message produced if a coercion is tried *)
let coercion_err = "Operations with different types are not supported"

(** The error message produced if an index of the array is not integer *)
let index_err = "The index is not of type INT"

(** The error message produced if the deference operator is not a pointer *)
let pointer_err = "The operator is not a pointer"


(** The error message produced during the initialization of a variable*)
let codegen_vardec_err = "The variable cannot be initialized with this type"