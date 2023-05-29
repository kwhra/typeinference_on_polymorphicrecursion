type t = Type_var.t list

let tyvarlist:t ref = ref []

let add tyvar = 
  let temp = !tyvarlist in
  tyvarlist := tyvar::temp

let reset () = tyvarlist:=[]

let is_mem x = List.mem x !tyvarlist