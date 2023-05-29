type t = int

let counter = ref 0

let incr () = 
  let value = !counter in
  counter := value+1

let get () = !counter

let set var = counter:=var

let reset () = counter := 0

let to_str = string_of_int