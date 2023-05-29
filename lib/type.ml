(* type = type variable
 *      | type constant
 *      | arrow
 *)

type var = Type_var.t

type con = 
  | Unit
  | Bool
  | Int

type t = 
  | Var of var
  | Con of con
  | Arr of t * t
  | Tuple of t * t
  | Variant of (Tag.t * t) list

module VarSet = Set.Make(struct type t = var let compare = compare end)

module VarMap = Map.Make(struct type t = var let compare = compare end)

let rec vars_of = function
  | Var x -> VarSet.singleton x
  | Con _ -> VarSet.empty
  | Arr (t1, t2) -> VarSet.union (vars_of t1) (vars_of t2)
  | Tuple (t1, t2) -> VarSet.union (vars_of t1) (vars_of t2)
  | Variant field ->
    List.fold_right
    (fun (_, t) set -> VarSet.union (vars_of t) set) field VarSet.empty 

let is_var_of x t = VarSet.mem x (vars_of t)

let is_var = function
  | Var _ -> true
  | _ -> false

exception IsNotVar

let to_var = function
  | Var x -> x
  | _ -> raise IsNotVar

(* increment, then get *)
let get () = 
  Type_var.incr();
  Var (Type_var.get())

let make_arrow t1 t2 = Arr(t1, t2)

let make_tuple t1 t2 = Tuple(t1, t2)

exception TagNotFound
let find_variant tag = function
  | Variant field when List.mem_assoc tag field -> List.assoc tag field
  | _ -> raise TagNotFound

let con_to_str = function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"

let con_to_tex = function
  | Unit -> "\\text{Unit}"
  | Bool -> "\\text{Bool}"
  | Int -> "\\text{Int}"

let rec to_str = function
  | Var x -> string_of_int x
  | Con c -> con_to_str c
  | Arr (t1, t2) ->
    let str1 = match t1 with
      | Arr _ -> "(" ^ (to_str t1) ^ ")"
      | _ -> (to_str t1)
    in
    let str2 = (to_str t2) in
    str1 ^ "->" ^ str2
  | Tuple (t1, t2) -> "{" ^ (to_str t1) ^ "," ^ (to_str t2) ^ "}"
  | Variant field ->
    let str = 
      List.fold_right
      (fun (tag, t) str -> (Tag.to_str tag) ^ ":" ^ (to_str t) ^ "," ^ str )
      field ""
    in
    "<" ^ str ^ ">"

let rec to_tex = function
  | Var x -> Type_var.to_str x
  | Con c -> con_to_tex c
  | Arr (t1, t2) ->
    let tex1 = match t1 with
      | Arr _ -> "(" ^ (to_tex t1) ^ ")"
      | _ -> (to_tex t1)
    in
    let tex2 = (to_tex t2) in
    tex1 ^ "\\rightarrow " ^ tex2
  | Tuple (t1, t2) -> "\\{" ^ (to_tex t1) ^ "," ^ (to_tex t2) ^ "\\}"
  | Variant field ->
    let tex = 
      List.fold_right
      (fun (tag, t) str -> (Tag.to_tex tag) ^ ":" ^ (to_tex t) ^ "," ^ str )
      field ""
    in
    "\\langle " ^ tex ^ "\\rangle "