(* constraint = (type * type) list, meaning T1 = T2 *)

type t = (Type.t * Type.t) list

let empty:t = []

let add t1 t2 (crnt:t) = (t1, t2)::crnt

let singleton t1 t2 = add t1 t2 empty

let merge c1 c2 :t= c1@c2

let map f (crnt:t) = List.map f crnt

let fold f (crnt:t) x= List.fold_right f crnt x

let of_type_envs env1 env2:t= 
  let intersection = 
    Term.VarMap.merge
    (fun _ v1 v2 -> match v1, v2 with
    | Some x, Some y -> Some(x, y)
    | _ -> None) env1 env2
  in
  let seq = Term.VarMap.to_seq intersection in
  let value_seq = Seq.map snd seq in
  List.of_seq value_seq

let to_str crnt = 
  let func (t1, t2) = (Type.to_str t1) ^ "=" ^ (Type.to_str t2) ^ "," in
  fold (fun eq str -> (func eq) ^ str) crnt ""

let to_tex crnt = 
    let func (t1, t2) = (Type.to_tex t1) ^ "=" ^ (Type.to_tex t2) ^ "," in
    fold (fun eq str -> (func eq) ^ str) crnt ""