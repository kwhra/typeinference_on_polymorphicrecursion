let type_of_con = function
  | Term.Unit -> Type.Con Type.Unit
  | Term.Bool -> Type.Con Type.Bool
  | Term.Int -> Type.Con Type.Int
  | Term.Ite -> 
    let ty = Type.get() in
    Type.make_arrow (Type.Con Type.Bool) (Type.make_arrow ty (Type.make_arrow ty ty))
  | Term.Fst -> 
    let ty1 = Type.get() in let ty2 = Type.get() in
    Type.make_arrow (Type.make_tuple ty1 ty2) ty1
  | Term.Snd ->
    let ty1 = Type.get() in let ty2 = Type.get() in
    Type.make_arrow (Type.make_tuple ty1 ty2) ty2
  

let rec_count = ref 0

let set_rec n = rec_count:=n

let get_rec () = !rec_count

exception RecTerm of Term.t
let rec make_ctree typing_env term = 
  match term with
  (* var *)
  | Term.Var x when not (Typing_env.mem x typing_env) -> 
    let typing = 
      let ty = Type.get () in
      let env = Type_env.singleton x ty in
      Typing.make env ty 
    in
    Cond_tree.make typing_env term typing []
  (* var-p *)
  | Term.Var x ->
    let typing = 
      let typing = Typing_env.find x typing_env in
      Rename.do_typing typing
    in
    Cond_tree.make typing_env term typing []
  (* con *)
  | Term.Con c ->
    let typing =
      let ty = type_of_con c in
      let env = Type_env.empty in
      Typing.make env ty
    in
    Cond_tree.make typing_env term typing []
  (*abs*)
  | Term.Abs (x1, t2) when Term.is_free_var_of x1 t2 ->
    let tree2 = make_ctree typing_env t2 in
    let typing = 
      let env2 = Cond_tree.to_type_env tree2 in
      let env = 
        Type_env.remove x1 env2
      in
      let ty = 
        let ty1 = Type_env.find x1 env2 in
        let ty2 = Cond_tree.to_ty tree2 in
        Type.make_arrow ty1 ty2
      in
      Typing.make env ty
    in
    Cond_tree.make typing_env term typing [tree2]
  (* abs vac *)
  | Term.Abs (_, t2) ->
    let tree2 = make_ctree typing_env t2 in
    let typing = 
      let env = Cond_tree.to_type_env tree2 in
      let ty = 
        let ty1 = Type.get() in
        let ty2 = Cond_tree.to_ty tree2 in
        Type.make_arrow ty1 ty2
      in
      Typing.make env ty
    in
    Cond_tree.make typing_env term typing [tree2]
  | Term.App (t1, t2) ->
    let tree1 = make_ctree typing_env t1 in
    let tree2 = make_ctree typing_env t2 in
    let env1 = Cond_tree.to_type_env tree1 in
    let env2 = Cond_tree.to_type_env tree2 in
    let ty = Type.get () in
    let typing = 
      let typing =
        let env = Type_env.union env1 env2 in
        Typing.make env ty
      in
      let crnt = 
        let crnt = Crnt.of_type_envs env1 env2 in
        let ty1 = Cond_tree.to_ty tree1 in
        let ty2 = Cond_tree.to_ty tree2 in
        (* t1:ty1 t2:ty2 => t1 t2: ty, where ty1 = ty2 -> ty *)
        Crnt.add ty1 (Type.make_arrow ty2 ty) crnt
      in
      Unifier.do_typing typing crnt
    in
    Cond_tree.make typing_env term typing [tree1; tree2]
  | Term.Rec (x1, t2) -> 
    let success = ref false in
    let typing_ref = ref Typing.temp in
    let childs = 
      let typing0 = Typing.base_of_tmvarset (Typing_env.fvs_of term typing_env) in
      let rec loop k typing_env x typing term = 
        if k = 0
          then []
          else
            (*D,x1:t0|-*)
            let tree = 
              let new_typing_env = Typing_env.add x typing typing_env in
              make_ctree new_typing_env term 
            in
            let new_typing = Cond_tree.to_typing tree in
            if Rename.is_eq typing new_typing
              then
                (success := true;
                typing_ref := new_typing;
                [tree])
              else
                tree::(loop (k-1) typing_env x new_typing term)
      in
      loop (get_rec()) typing_env x1 typing0 t2
    in
    if !success
      then
        Cond_tree.make typing_env term !typing_ref childs
      else
        let tree2 = make_ctree typing_env t2 in
        let cond = Cond_tree.to_cond tree2 in
        let ty = Condition.to_ty cond in
        let type_env = Condition.to_type_env cond in
        if ty = (Type_env.find x1 type_env)
          then 
            let typing = 
              let type_env = Type_env.remove x1 type_env in
              Typing.make type_env ty
            in
            Cond_tree.make typing_env term typing [tree2]
          else 
            raise (RecTerm term)
  (*Tuple*)
  | Term.Tuple (t1, t2) ->
    let tree1 = make_ctree typing_env t1 in
    let tree2 = make_ctree typing_env t2 in
    let typing = 
      let env1 = Cond_tree.to_type_env tree1 in
      let env2 = Cond_tree.to_type_env tree2 in
      let typing = 
        let env = Type_env.union env1 env2 in
        let ty =
          let ty1 = Cond_tree.to_ty tree1 in
          let ty2 = Cond_tree.to_ty tree2 in
          Type.make_tuple ty1 ty2
        in
        Typing.make env ty
      in
      let crnt = Crnt.of_type_envs env1 env2 in
      Unifier.do_typing typing crnt
    in
    Cond_tree.make typing_env term typing [tree1;tree2]
  (*Variant*)
  | Term.Variant (tag, tm1) ->
    let tree1 = make_ctree typing_env tm1 in
    let typing = 
      let typing = 
        let env = Cond_tree.to_type_env tree1 in
        let ty = Tag_world.find tag in
        Typing.make env ty
      in
      let crnt = 
        let ty1 = Cond_tree.to_ty tree1 in
        let ty2 = Type.find_variant tag (Tag_world.find tag) in
        Crnt.singleton ty1 ty2
      in
      Unifier.do_typing typing crnt
    in
    Cond_tree.make typing_env term typing [tree1]
  | Term.CaseOf (t0, field) ->
    let tree0 = make_ctree typing_env t0 in
    let treelist = 
      List.map
      (fun (tag, x, t) -> (tag, x, make_ctree typing_env t)) field
    in
    let typing = 
      let ty = Type.get() in
      let tyenv = 
        List.fold_right
        (fun (_, x, tree) env -> 
          Type_env.union
          (Type_env.remove x (Cond_tree.to_type_env tree)) env)
          treelist (Cond_tree.to_type_env tree0)
      in
      let typing = Typing.make tyenv ty in
      let crnt =
        (*<li=xi>ti, each xi is same type as <li:Ti> in variant type *)
        let crnt1 = 
          let ty0 = Cond_tree.to_ty tree0 in
          let variant = 
            Type.Variant
            (List.map
              (fun (tag, x, tree) ->
                try
                  let env = Cond_tree.to_type_env tree in
                  (tag, Type_env.find x env)
                with
                Not_found -> (tag, Type.get()))
              treelist)
          in
          Crnt.singleton ty0 variant
        in
        (*<li=xi> ti, all ti are same type as "case of ..."*)
        let crnt2 = 
          List.fold_right
          (fun (_, _, tree) crnt -> 
            Crnt.add (Cond_tree.to_ty tree) (ty) crnt)
          treelist Crnt.empty
        in
        (*constraint from all type env i and type env 1*)
        let crnt3 = 
          List.fold_right
          (fun (_, x, tree) crnt -> 
            let env = Type_env.remove x (Cond_tree.to_type_env tree) in
            Crnt.merge (Crnt.of_type_envs env tyenv) crnt)
          treelist Crnt.empty
        in
        Crnt.merge crnt1 (Crnt.merge crnt2 crnt3)
      in
      Unifier.do_typing typing crnt
    in
    let treelist = List.map (fun (_, _, tree) -> tree) treelist in
    Cond_tree.make typing_env term typing (tree0::treelist)
