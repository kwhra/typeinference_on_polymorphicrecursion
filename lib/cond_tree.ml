type t = Condition.t Tree.t

let make typing_env term typing childs:t = Tree.make (typing_env, term, typing) childs

let to_cond (tree:t) = Tree.to_parent tree

let to_typing (tree:t) = Condition.to_typing(Tree.to_parent tree)

let to_type_env (tree:t) = Condition.to_type_env(Tree.to_parent tree)

let to_ty (tree:t) = Condition.to_ty(Tree.to_parent tree)

let to_childs (tree:t) = Tree.to_childs tree

let fold_from_root f (tree:t) init = Tree.fold_from_root f tree init

let map f (t:t):t = Tree.map f t

let to_str (tree:t) = 
  let func cond str = 
    let (_, term, _) = cond in
    let rule = match term with
      | Term.Var _ -> "(Var)"
      | Term.Con _ -> "(Con)"
      | Term.Abs _ -> "(Abs)"
      | Term.App _ -> "(App)"
      | Term.Rec _ -> "(Rec)"
      | Term.Tuple _ -> "(Tuple)"
      | Term.Variant _ -> "(Variant)"
      | Term.CaseOf _ -> "(Case)"
    in
    rule ^ "\n" ^ (Condition.to_str cond)  ^ "\n" ^ str
  in
  fold_from_root func tree ""

let to_tex (tree:t) = 
  let func cond init = 
    let (_, term, _) = cond in
    let str = match term with
      | Term.Var _ -> "\\AxiomC{}\n" ^ "\\RightLabel{(Var)}\n" ^ "\\UnaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
      | Term.Con _ -> "\\AxiomC{}\n" ^ "\\RightLabel{(Con)}\n" ^ "\\UnaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
      | Term.Abs _ ->                  "\\RightLabel{(Abs)}\n" ^ "\\UnaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
      | Term.App _ ->                  "\\RightLabel{(App)}\n" ^"\\BinaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
      | Term.Rec _ -> "%rec\n\\AxiomC{}\n" ^ "\\RightLabel{(Rec)}\n" ^ "\\UnaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
      | Term.Tuple _ ->                "\\RightLabel{(Tuple)}\n" ^"\\BinaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
      | Term.Variant _ ->              "\\RightLabel{(Variant)}\n" ^ "\\UnaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
      | Term.CaseOf _ -> "case\n\\AxiomC{}" ^ "\\RightLabel{(Case)}\n" ^ "\\UnaryInfC{ $"  ^ (Condition.to_str cond) ^ "$ }\n"
    in
    str ^ init
  in
  fold_from_root func tree ""
