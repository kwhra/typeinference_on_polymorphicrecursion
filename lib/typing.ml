(* typing = <type environment, type>*)

type t = Type_env.t * Type.t

let base_of_tmvarset set:t = 
  let type_env = 
    Term.VarSet.fold
      (fun tmvar env -> 
        Type_env.add 
          tmvar (Type.get()) env)
      set Type_env.empty
  in
  let ty = Type_var.incr();Type.Var(Type_var.get()) in
  (type_env, ty)

let make env ty:t = (env, ty)

let temp:t = make (Type_env.empty) (Type.Con Type.Unit)

let to_ty ((_, ty):t) = ty

let to_type_env ((env, _):t) = env

let to_str ((env, t):t) = "<" ^ (Type_env.to_str env) ^ "; " ^ (Type.to_str t) ^ ">"

let to_tex ((env, t):t) = "\\langle" ^ (Type_env.to_tex env) ^ "; " ^ (Type.to_tex t) ^ "\\rangle"