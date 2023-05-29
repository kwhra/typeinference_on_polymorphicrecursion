(* type environment = (term variable: type) list *)

type t = Type.t Term.VarMap.t

let empty:t = Term.VarMap.empty

let singleton:_ -> _ -> t = Term.VarMap.singleton

let add:_->_->t->t = Term.VarMap.add

let union:_ -> _ -> t = Term.VarMap.merge Util.former

let mem:_ -> t -> _ = Term.VarMap.mem

let find x (env:t) = Term.VarMap.find x env

let remove x (env:t) = Term.VarMap.remove x env

let map:_->t->t = Term.VarMap.map

let fold:_ -> t -> _ -> _ = Term.VarMap.fold

let dom (env:t) = 
  let seq = Term.VarMap.to_seq env in
  let key_seq = Seq.map fst seq in
  Term.VarSet.of_seq key_seq

let image (env:t) = 
  let seq = Term.VarMap.to_seq env in
  let value_seq = Seq.map snd seq in
  List.of_seq value_seq

let to_str env = 
  let func x t = x ^ ":" ^ (Type.to_str t) ^ "," in
  fold (fun x t str -> (func x t) ^ str) env ""

let to_tex env = 
  let func x t = x ^ ":" ^ (Type.to_tex t) ^ "," in
  fold (fun x t str -> (func x t) ^ str) env ""
