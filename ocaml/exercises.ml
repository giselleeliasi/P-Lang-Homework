exception Negative_Amount

let change amount =
  if amount < 0 then
    raise Negative_Amount
  else
    let denominations = [25; 10; 5; 1] in
    let rec aux remaining denominations =
      match denominations with
      | [] -> []
      | d :: ds -> (remaining / d) :: aux (remaining mod d) ds
    in
    aux amount denominations

let first_then_apply array predicate consumer =
  match List.find_opt predicate array with 
  | None -> None
  | Some x -> consumer x


let powers_generator base =
  let rec generator_from power () =
    Seq.Cons (power, generator_from (power * base))
  in
  generator_from 1;;

let meaningful_line_count (filename: string) : int =
  let count_lines_in_channel channel =
    let rec aux acc =
      try
        let line = input_line channel in
        let trimmed_line = String.trim line in
        if trimmed_line <> "" && not (String.starts_with ~prefix:"# " trimmed_line) then
          aux (acc + 1) 
        else
          aux acc  
      with End_of_file ->
        acc  
    in
    aux 0
  in
  let in_channel = open_in filename in
  Fun.protect
    (fun () -> count_lines_in_channel in_channel) 
    ~finally:(fun () -> close_in in_channel)  


type shape =
  | Sphere of float 
  | Box of float * float * float  

let volume s =
  match s with
  | Sphere r -> (4.0 /. 3.0) *. Float.pi *. (r ** 3.0) 
  | Box (l, w, h) -> l *. w *. h 

let surface_area s =
  match s with
  | Sphere r -> 4.0 *. Float.pi *. (r ** 2.0) 
  | Box (l, w, h) -> 2.0 *. (l *. w +. l *. h +. w *. h)  

let to_string s =
  match s with
  | Sphere r -> Printf.sprintf "Sphere(radius: %.2f)" r
  | Box (l, w, h) -> Printf.sprintf "Box(length: %.2f, width: %.2f, height: %.2f)" l w h


type 'a binary_search_tree =
  | Empty
  | Node of 'a binary_search_tree * 'a * 'a binary_search_tree

let rec insert x tree =
  match tree with
  | Empty -> Node (Empty, x, Empty)
  | Node (left, value, right) ->
      if x < value then
        Node (insert x left, value, right)  
      else if x > value then
        Node (left, value, insert x right) 
      else
        tree  

let rec lookup x tree =
  match tree with
  | Empty -> false
  | Node (left, value, right) ->
      if x < value then
        lookup x left  
      else if x > value then
        lookup x right 
      else
        true  

let contains x tree =
  lookup x tree  

let rec size tree =
  match tree with
  | Empty -> 0
  | Node (left, _, right) -> 1 + size left + size right

let rec inorder tree =
  match tree with
  | Empty -> []
  | Node (left, value, right) -> inorder left @ [value] @ inorder right
