type t = Type.t Tag.TagMap.t

let memory:t ref  = ref (Tag.TagMap.empty)

exception NotVariantType
let push ty = 
  match ty with
  | Type.Variant list -> 
      List.fold_right 
        (fun (tag, _) -> fun () -> 
          memory := Tag.TagMap.add tag ty (!memory))
      list ()
  | _ -> raise NotVariantType

let find tag = 
  Tag.TagMap.find tag !memory

let reset () = memory := Tag.TagMap.empty