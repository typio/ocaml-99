let len l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | _ :: t -> aux t (acc)
  in
  aux l 0
    
(* non-tail recursive
let rec len l =
  match l with
  | [] -> 0
  | _ :: xs -> 1 + len xs
*)