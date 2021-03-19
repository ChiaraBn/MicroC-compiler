exception DuplicateEntry
exception NotFoundEntry


module STable = Map.Make(String);;

type 'a t = ('a STable.t) list

let empty_table = [ STable.empty ]

let begin_block table = (STable.empty)::table

let end_block table = 
  match table with
  | [] -> []
  | x::xs -> xs

(** symbol: string, info: map, table: list map *)
let rec add_entry symbol info table = 
  match table with 
  | []    -> let e = empty_table in add_entry symbol info e

  | x::xs -> (match (STable.find_opt symbol x) with
              |None -> (STable.add symbol info x)::xs
              |Some(x) -> raise DuplicateEntry
             )

(** symbol: string, table: list map *)
let rec lookup symbol table = 
  match table with
  | []    -> raise NotFoundEntry
  | x::xs -> (match (STable.find_opt symbol x) with
              | None -> lookup symbol xs
              | Some(x) -> x
            )

(** TODO Cancella anche in mli *)
let rec print_elems (table :'a t) = 
  match table with
  | [] -> Printf.fprintf stderr "Fine tabella \n"
  | x::xs ->  let t1 = STable.bindings x in
              ignore( List.iter (fun (a,b) -> Printf.fprintf stderr "%s \n" a ) t1);
            print_elems xs