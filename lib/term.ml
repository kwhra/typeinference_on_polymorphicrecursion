(* term = term variant
 *      | term constant
 *      | term abstract
 *      | term application
 *      | rec *)

type var = string

type con =
  | Unit
  | Bool
  | Int
  | Ite
  | Fst
  | Snd

type t = 
  | Var of var
  | Con of con
  | Abs of var * t
  | App of t * t
  | Rec of var * t
  | Tuple of t * t
  | Variant of Tag.t * t 
  | CaseOf of t * (Tag.t * var * t) list

module VarSet = Set.Make(struct type t = var let compare = compare end)

module VarMap = Map.Make(struct type t = var let compare = compare end)

let rec free_vars_of = function
  | Var x -> VarSet.singleton x
  | Con _ -> VarSet.empty
  | Abs (x, t) ->VarSet.remove x (free_vars_of t)
  | App (t1, t2) -> VarSet.union (free_vars_of t1) (free_vars_of t2)
  | Rec (x, t) -> VarSet.remove x (free_vars_of t)
  | Tuple (t1, t2) -> VarSet.union (free_vars_of t1) (free_vars_of t2)
  | Variant (_, tm) -> free_vars_of tm
  | CaseOf (t1, field) ->
    let set = 
      List.fold_right
      (fun (_, x, t) set -> VarSet.union (VarSet.remove x (free_vars_of t)) set)
      field VarSet.empty
    in
    VarSet.union (free_vars_of t1) set

let is_free_var_of x t = VarSet.mem x (free_vars_of t)

let con_to_str = function
    | Unit -> "unit"
    | Bool -> "bool"
    | Int -> "integer"
    | Ite -> "ite"
    | Fst -> "fst"
    | Snd -> "snd"

let con_to_tex = function
| Unit -> "\\text{unit}"
| Bool -> "\\text{bool}"
| Int -> "\\text{integer}"
| Ite -> "\\text{ite}"
| Fst -> "\\text{fst}"
| Snd -> "\\text{Snd}"

let rec to_str = function
  | Var x -> x
  | Con c -> con_to_str c
  | Abs (x, t) -> "\\" ^ x ^ "." ^ (to_str t)
  | App (t1, t2) -> 
    let str1 = match t1 with
      | Abs _ -> "(" ^ (to_str t1) ^ ")"
      | Rec _ -> "(" ^ (to_str t1) ^ ")"
      | _ -> to_str t1
    in
    let str2 = match t2 with
      | App _ -> "(" ^ (to_str t2) ^ ")"
      | _ -> (to_str t2)
    in
    str1 ^ " " ^ str2
  | Rec (x, t) -> "rec " ^ x ^ "." ^ (to_str t)
  | Tuple (t1, t2) -> "{" ^ (to_str t1) ^ "," ^ (to_str t2) ^ "}"
  | Variant (tag, tm) -> "<" ^ (Tag.to_str tag) ^ "=" ^  (to_str tm) ^ ">"
  | CaseOf (t1, field) ->
    let str =
      List.fold_right
      (fun (tag, x, t) str -> "<" ^ (Tag.to_str tag) ^ "=" ^ x ^ "> " ^ (to_str t) ^ "|" ^ str )
      field ""
    in
    "case " ^ (to_str t1) ^ " of " ^ str

let rec to_tex = function
  | Var x -> x
  | Con c -> con_to_tex c
  | Abs (x, t) -> "\\backslash " ^ x ^ "." ^ (to_tex t)
  | App (t1, t2) -> 
    let str1 = match t1 with
      | Abs _ -> "(" ^ (to_tex t1) ^ ")"
      | Rec _ -> "(" ^ (to_tex t1) ^ ")"
      | _ -> to_tex t1
    in
    let str2 = match t2 with
      | App _ -> "(" ^ (to_tex t2) ^ ")"
      | _ -> (to_tex t2)
    in
    str1 ^ " " ^ str2
  | Rec (x, t) -> "\\text{rec}" ^ x ^ "." ^ (to_tex t) ^ ")"
  | Tuple (t1, t2) -> "\\{" ^ (to_tex t1) ^ "," ^ (to_tex t2) ^ "\\}"
  | Variant (tag, tm) -> "\\langle " ^ (Tag.to_tex tag) ^ "=" ^ (to_tex tm) ^ "\\rangle"
  | CaseOf (t1, field) ->
    let tex =
      List.fold_right
      (fun (tag, x, t) tex -> "\\langle " ^ (Tag.to_str tag) ^ "=" ^ x ^ "\\rangle " ^ (to_str t) ^ "|" ^ tex )
      field ""
    in
    "\\text{case}\\," ^ (to_str t1) ^ "\\,\\text{of}\\, " ^ tex