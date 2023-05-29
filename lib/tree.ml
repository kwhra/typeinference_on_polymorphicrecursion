(* constructor *)
type 'a t = Node of 'a * 'a t list

let make parent childs = Node(parent, childs)

let empty_child:('a t list) = []

let add_childs t tl :('a t list)= t::tl

let to_parent tree = 
  let Node(parent, _) = tree in parent

let to_childs tree = 
  let Node(_, childs) = tree in childs

let rec map func tree = 
  let Node (some, childs) = tree in
  match childs with
  | [] -> Node (func some, [])
  (* hd: 'a tree, tl: 'a tree list *)
  | hd::tl ->
    let newhead = map func hd in
    let Node (newsome, newtl) = map func (Node(some, tl)) in
    Node (newsome, newhead::newtl)

(* function done from tail to head *)
(* ex: Node(0, [1;2;3]) => f 1(f 2(f 3(f 0 init))) *)
let rec fold_from_root func (Node (parent, child)) init = 
  match child with
  | [] -> func parent init
  | hd::tl ->
    let xtl = fold_from_root func (Node (parent, tl)) init in
    fold_from_root func hd xtl