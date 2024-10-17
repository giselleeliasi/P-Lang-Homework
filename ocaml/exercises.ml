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

(* Write your powers generator here *)
let powers_generator base =
  let rec generator_from power () =
    Seq.Cons (power, generator_from (power * base))
  in
  generator_from 1;;

(* Write your line count function here *)

(* Write your shape type and associated functions here *)

(* Write your binary search tree implementation here *)
