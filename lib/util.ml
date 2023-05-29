(* select former value if same key *)
let former = fun _ v1 v2 -> match v1, v2 with
| Some x, Some _ -> Some x
| Some x, None -> Some x
| None, Some y -> Some y
| _ -> None

let raising = 
  fun _ v1 v2 -> match v1, v2 with
  | Some _, Some _ -> failwith "same key"
  | Some x, None -> Some x
  | None, Some y -> Some y
  | _ -> None