type t = string

module TagMap = Map.Make(struct type t = string let compare = compare end)

let to_str (tag:t):string = tag

let to_tex (tag:t):string = tag