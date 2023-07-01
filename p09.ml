(* # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]] *)

let pack l = 
  let rec aux l final_list current_sublist =
    match l with
    | [] -> (current_sublist) :: final_list
    | [x] -> (x :: current_sublist) :: final_list
    | x :: (y :: _ as xs) -> 
      if x = y then
        aux xs final_list (x :: current_sublist)
      else
        aux xs ((x :: current_sublist) :: final_list) []
  in
  match l with
  | [] -> []
  | _ -> List.rev (aux l [] [])
