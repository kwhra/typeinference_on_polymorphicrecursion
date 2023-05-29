(* rename substitution: tyvar -> tyvar *)

type t = (Type_var.t * Type_var.t) list

let empty:t = []

let singleton t1 t2 = (t1, t2)::empty

let rec mem x (renamer:t) = match renamer with
  | [] -> false
  | (t1, _)::tl -> 
    if x = t1 then true
    else mem x tl

exception Notfound
let rec find x (renamer:t) = match renamer with
  | [] -> raise Notfound
  | (t1, t2)::tl -> 
    if x = t1 then t2
    else find x tl

let rec to_str = function
  | [] -> ""
  | (ty1, ty2)::tl ->
    let str = (Type_var.to_str ty1) ^ " |-> " ^ (Type.to_str (Type.Var ty2)) ^ ", " in
    str ^ (to_str tl)

let renamer_of_ty ty = 
  (* Type.t -> t *)
  let rec loop ty0 = 
    match ty0 with
    | Type.Var x when not (Tyvarlist.is_mem x) -> 
      Tyvarlist.add x; 
      Type_var.incr();
      singleton x (Type_var.get())
    | Type.Var _ -> empty
    | Type.Con _ -> empty
    | Type.Arr (t1, t2) ->
      let sub1 = loop t1 in
      let sub2 = loop t2 in
      sub1@sub2
    | Type.Tuple (t1, t2) ->
      let sub1 = loop t1 in
      let sub2 = loop t2 in
      sub1@sub2
    | Type.Variant field -> 
      List.fold_left
        (fun subst (_, ty) -> subst @ (loop ty)) empty field
  in
  Tyvarlist.reset();
  loop ty

let subst_for_ty renamer ty = 
  (* t -> Labelled_type.t -> Labelled_type.t *)
  let rec subst_for_lty renamer lty = 
    let rec replace (oldty, newty) dst = 
      match dst with
      | Labelled_type.Var (x, is_renamed) when not(is_renamed) && (x = oldty) ->
        Labelled_type.Var (newty, true)
      | Labelled_type.Var _ -> dst
      | Labelled_type.Con _ -> dst
      | Labelled_type.Arr (t1, t2) -> Labelled_type.Arr (replace (oldty, newty) t1, replace (oldty, newty) t2)
      | Labelled_type.Tuple (t1, t2) -> Labelled_type.Tuple (replace (oldty, newty) t1, replace (oldty, newty) t2)
      | Labelled_type.Variant field ->
        let field = 
          List.map
          (fun (tag, ty) -> (tag, replace (oldty, newty) ty)) field
        in
        Variant field
      
    in
    match renamer with
    | [] -> lty
    | hd::tl ->
      let newlty = replace hd lty in
      subst_for_lty tl newlty
  in
  let lty = Labelled_type.of_type ty in
  let newlty = subst_for_lty renamer lty in
  Labelled_type.to_type newlty

let subst_for_type_env subst env = 
  Type_env.map (fun ty -> subst_for_ty subst ty) env

let do_type ty = 
  let renamer = renamer_of_ty ty in
  subst_for_ty renamer ty

let do_typing ((env, ty):Typing.t) = 
  let renamer = renamer_of_ty ty in
  (subst_for_type_env renamer env, subst_for_ty renamer ty)

let is_eq typing1 typing2 = 
  try
    let temp = Type_var.get() in
    Type_var.reset();
    let new1 = do_typing typing1 in
    Type_var.reset();
    let new2 = do_typing typing2 in
    Type_var.set temp;
    new1 = new2
  with
  | _ -> failwith "eval equality"