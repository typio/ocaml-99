type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | One x :: xs -> aux xs (x :: acc)
    | Many xs :: ys -> aux (xs @ ys) acc

  in
  List.rev (aux l [])