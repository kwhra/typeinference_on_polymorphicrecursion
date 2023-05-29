(* typing environment = (term variiable: typing) list*)

type t = Typing.t Term.VarMap.t

let empty:t = Term.VarMap.empty

let singleton:_ -> _ -> t = Term.VarMap.singleton

let add x t (env:t) = Term.VarMap.add x t env

let mem:_ -> t -> _ = Term.VarMap.mem

let find:_-> t -> _ = Term.VarMap.find

let union:t -> t -> t = Term.VarMap.merge Util.former

let fold f (env:t) x = Term.VarMap.fold f env x

let dom (env:t) = 
  let seq = Term.VarMap.to_seq env in
  let key_seq = Seq.map fst seq in
  Term.VarSet.of_seq key_seq

let image_tmvars (env:t) = 
  let seq = Term.VarMap.to_seq env in
  let value_seq = Seq.map snd seq in
  let env_seq = Seq.map fst value_seq in
  let varset_seq = Seq.map Type_env.dom env_seq in
  Seq.fold_left Term.VarSet.union Term.VarSet.empty varset_seq

let fvs_of term env = 
  Term.VarSet.union
    (Term.VarSet.diff (Term.free_vars_of term) (dom env))
    (image_tmvars env)

let is_fv_of x term env = 
  Term.VarSet.mem x (fvs_of term env)

(* if x in dom D or image D then true: false otherwize *)
let included x env = 
  (Term.VarSet.mem x (dom env)) || (Term.VarSet.mem x (image_tmvars env))
  
let to_str env = 
  let func x t = x ^ ":" ^ (Typing.to_str t) ^ "," in
  fold (fun x t str -> (func x t) ^ str) env ""

let to_tex env = 
  let func x t = x ^ ":" ^ (Typing.to_tex t) ^ "," in
  fold (fun x t str -> (func x t) ^ str) env ""
  