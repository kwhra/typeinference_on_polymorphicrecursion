(* t = (typing env E, term t, typing T), meaning E|-t:T *)

type t = Typing_env.t * Term.t * Typing.t

let make env term typing:t = (env, term, typing)

let to_typing_env ((env, _, _):t) = env

let to_term ((_, term, _):t) = term

let to_typing ((_, _, typing):t) = typing

let to_ty ((_, _, (_, ty)):t) = ty

let to_type_env ((_, _, (env, _)):t) = env

let to_str ((env, term, typing):t) = (Typing_env.to_str env) ^ " |- " ^ (Term.to_str term) ^ " : " ^ (Typing.to_str typing)

let to_tex ((env, term, typing):t) = (Typing_env.to_tex env) ^ "\\vdash " ^ (Term.to_tex term) ^ " : " ^ (Typing.to_tex typing)