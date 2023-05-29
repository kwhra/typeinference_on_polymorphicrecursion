open Lib

let case_unification = [
  (Crnt.empty, Subst.empty);
  (Crnt.singleton (Type.Arr(Con Unit, Con Unit)) (Type.Var 0), Subst.singleton 0 (Type.Arr(Con Unit, Con Unit)));
  (Crnt.singleton (Type.Arr(Con Unit, Con Unit)) (Type.Var 0), 
    Subst.singleton 0 (Type.Arr(Con Unit, Con Unit)));
]

let test_unification = 
  List.fold_right (fun (crnt, subst) -> fun bool -> Unifier.of_crnt crnt = subst && bool) case_unification true

let case_inference = [
  (1, "rec x. x x", Type_env.empty, Type.Var 0);

  (* negative example *)
  (2, "rec ff. \\g.\\y. ite false y (g(ff g y))", Type_env.empty, 
  let alpha = Type.Var 0 in Type.make_arrow (Type.make_arrow alpha alpha) (Type.make_arrow alpha alpha));

  (3, "rec f. \\g.\\y. ite false y (g(f g y))", Type_env.empty, 
  let alpha = Type.Var 0 in Type.make_arrow (Type.make_arrow alpha alpha) (Type.make_arrow alpha alpha));

  (* (0, "<`none = unit> as <`none:Unit>", Type_env.empty, Type.Variant [("`none", Type.Con Type.Unit)]); *)

  (0, "{1, true}", Type_env.empty, Type.make_tuple (Type.Con Type.Int) (Type.Con Type.Bool));

  (0, "\\x. case x of <`none=none> 0 | <`num=num> num ", Type_env.empty,
  let variant = Type.Variant [("`none", Type.Var 0); ("`num", Type.Con Type.Int)] in
  Type.make_arrow variant (Type.Con Type.Int));

  (0, "(\\x. case x of <`none=none> 0 | <`num=num> num) (<`none=unit> as <`none:Unit, `num:Int>)", 
  Type_env.empty, Type.Con Type.Int)
]

let rec test_inference = function
  | [] -> []
  | (num, str, type_env, ty)::tail ->
    let term = Parser.program Lexer.token (Lexing.from_string str) in
    try
      Infer.set_rec num;
      let typing = Cond_tree.to_typing (Infer.make_ctree Typing_env.empty term) in
      if Rename.is_eq typing (Typing.make type_env ty)
        then test_inference tail
      else str::(test_inference tail)
    with
    _ -> str::(test_inference tail)


let () =
  List.fold_right
  (fun str _ -> print_endline("fail with term: " ^ str)) (test_inference case_inference) ();
  if test_unification then print_endline("unification success") else print_endline("unification failed")