(* substitution *)

type t = Type.t Type.VarMap.t

let empty:t = Type.VarMap.empty

let singleton:_ -> _ -> t = Type.VarMap.singleton

let mem:_->t->_ = Type.VarMap.mem

let fold:_->t->_ ->_ = Type.VarMap.fold

let do_for_type (subst:t) (ty:Type.t) = 
  let rec replace var subty dstty = match dstty with
    | Type.Var x when var = x -> subty
    | Type.Var _ -> dstty
    | Type.Con _ -> dstty
    | Type.Arr (ty1, ty2) -> Type.Arr (replace var subty ty1, replace var subty ty2)
    | Type.Tuple (ty1, ty2) -> Type.Tuple (replace var subty ty1, replace var subty ty2)
    | Type.Variant field ->
      let field = 
        List.map
        (fun (tag, ty) -> (tag, replace var subty ty)) field
      in
      Type.Variant field
  in
  Type.VarMap.fold replace subst ty

let do_for_type_env subst env = Term.VarMap.map (do_for_type subst) env

let do_for_typing subst typing = 
  let env = 
    let env = Typing.to_type_env typing in
    do_for_type_env subst env in
  let ty = 
    let ty = Typing.to_ty typing in
    do_for_type subst ty in
  Typing.make env ty

let do_for_typing_env subst env = Term.VarMap.map (do_for_typing subst) env

let do_for_cond subst cond:Condition.t = 
  let typing_env = 
    let typing_env = Condition.to_typing_env cond in
    do_for_typing_env subst typing_env
  in
  let term = Condition.to_term cond in
  let typing = 
    let typing = Condition.to_typing cond in
    do_for_typing subst typing in
  Condition.make typing_env term typing

let do_for_crnt subst crnt:Crnt.t = 
  Crnt.map
    (fun (t1, t2) -> (do_for_type subst t1, do_for_type subst t2))
    crnt

let do_for_subst subst dstsubst = Type.VarMap.map (do_for_type subst) dstsubst

let merge sub1 sub2= Type.VarMap.merge Util.former (do_for_subst sub2 sub1) (do_for_subst sub1 sub2)


let to_str subst = 
  let func var ty str = (Type_var.to_str var) ^ "|->" ^ (Type.to_str ty) ^ ", " ^ str in
  fold func subst ""