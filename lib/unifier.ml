exception UnificationFailed of Type.t * Type.t

let rec of_crnt (crnt:Crnt.t) = 
  match crnt with
  | [] -> Subst.empty
  | (t1, t2)::tl ->
    if t1 = t2 then of_crnt tl
    else match t1 with
    | Var x1 when not (Type.is_var_of x1 t2) -> 
      let subst = Subst.singleton x1 t2 in
      let tl = Subst.do_for_crnt subst tl in
      Subst.merge subst (of_crnt tl)
    | Var _ -> raise (UnificationFailed (t1, t2))
    | Con _ -> 
      (match t2 with
      | Var x2 ->
        let subst = Subst.singleton x2 t1 in
        let tl = Subst.do_for_crnt subst tl in
        Subst.merge subst (of_crnt tl)
      | _ -> raise (UnificationFailed (t1, t2)))
    | Arr (t11, t12) -> 
      (match t2 with
      | Var x2 when not (Type.is_var_of x2 t1)-> 
        let subst = Subst.singleton x2 t1 in
        let tl = Subst.do_for_crnt subst tl in
        Subst.merge subst (of_crnt tl)
      | Arr (t21, t22) ->
        of_crnt ((t11, t21)::(t12, t22)::tl)
      | _ -> raise (UnificationFailed (t1, t2)))
    | Tuple (t11, t12) -> 
      (match t2 with
      | Var x2 when not (Type.is_var_of x2 t1)-> 
        let subst = Subst.singleton x2 t1 in
        let tl = Subst.do_for_crnt subst tl in
        Subst.merge subst (of_crnt tl)
      | Tuple (t21, t22) ->
        of_crnt ((t11, t21)::(t12, t22)::tl)
      | _ -> raise (UnificationFailed (t1, t2)))
    | Variant field1 ->
      (match t2 with
      | Var x2 when not (Type.is_var_of x2 t1) ->
        let subst = Subst.singleton x2 t1 in
        let tl = Subst.do_for_crnt subst tl in
        Subst.merge subst (of_crnt tl)
      | Variant field2 ->
        let crnt12 = 
          List.fold_right2
          (fun (tag1, ty1) (tag2, ty2) crnt ->
            if tag1 = tag2
              then Crnt.merge (Crnt.singleton ty1 ty2) crnt
              else raise (UnificationFailed(t1, t2)))
          field1 field2 Crnt.empty
        in
        of_crnt (Crnt.merge crnt12 crnt)
      | _ -> raise (UnificationFailed(t1, t2)))

let do_typing typing crnt = 
  let unifier = of_crnt crnt in
  Subst.do_for_typing unifier typing