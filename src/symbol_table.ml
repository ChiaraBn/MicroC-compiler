(**
  The module that implements the symbol table used by the
  type checker
*)

exception DuplicateEntry
exception NotFoundEntry


module STable = Map.Make(String);;

(** Implementing the symbol table as a list of maps *)
type 'a t = ('a STable.t) list

(** Function that creates an empty table *)
let empty_table = [ STable.empty ]

(** Function that adds a new block to the table
  @param table the initial table to be updated
*)
let begin_block table = (STable.empty)::table

(** Function that returns the last block of the table
  @param table
*)
let end_block table = 
  match table with
  | [] -> []
  | x::xs -> xs

(** Function that adds a new symbol into the table
  @param symbol the symbol to be added
  @param info the map of the symbol
  @param table the table to be updated
  @raise DuplicateEntry when the symbol is already in the table
*)
let rec add_entry symbol info table = 
  match table with 
  | []    -> let e = empty_table in add_entry symbol info e

  | x::xs -> (match (STable.find_opt symbol x) with
              |None -> (STable.add symbol info x)::xs
              |Some(x) -> raise DuplicateEntry
             )

(** Function that searches for a symbol in the table
  @param symbol the symbol to be searching for
  @param table
  @raise NotFoundEntry when the symbol is not in the table
*)
let rec lookup symbol table = 
  match table with
  | []    -> raise NotFoundEntry
  | x::xs -> (match (STable.find_opt symbol x) with
              | None -> lookup symbol xs
              | Some(x) -> x
            )