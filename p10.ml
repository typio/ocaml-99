(* # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)

let encode l = 
  let rec aux l final_list (count, (el: string)) =
    match l with
    | [] -> (count, el) :: final_list
    | x :: xs -> 
      if x = el then
         aux xs final_list (count + 1, el)
      else
        aux xs ((count, el) :: final_list) (1, x)
  in
  match l with
  | [] -> []
  | x :: xs -> List.rev (aux xs [] (1, x))
