type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten l =
  let rec aux l flat =
    match l with
    | [] -> flat
    | One x :: xs -> aux xs (flat @ [x])
    | Many x :: xs -> aux xs (flat @ aux x [])

  in
  aux l []