(* labelled type = type * bool *)
(* true: already renamed 
*  false:not yet *)

type t = 
  | Var of Type_var.t * bool
  | Con of Type.con
  | Arr of t * t
  | Tuple of t * t
  | Variant of (Tag.t * t) list

let rec of_type = function
  | Type.Var x -> Var (x, false)
  | Type.Con c -> Con c
  | Type.Arr (t1, t2) -> Arr(of_type t1, of_type t2)
  | Type.Tuple (t1, t2) -> Tuple(of_type t1, of_type t2)
  | Type.Variant field ->
    let field = 
      List.map
      (fun (tag, t) -> (tag, of_type t))
      field
    in
    Variant field
    

let rec to_type lty = match lty with
  | Var (x, is_renamed) when is_renamed -> Type.Var x
  | Con c -> Type.Con c
  | Arr (t1, t2) -> Type.Arr (to_type t1, to_type t2)
  | Tuple (t1, t2) -> Type.Tuple (to_type t1, to_type t2)
  | Variant field ->
    let field = 
      List.map
      (fun (tag, lty) -> (tag, to_type lty)) field
    in
    Type.Variant field
  | _ -> failwith ("Rename fail")