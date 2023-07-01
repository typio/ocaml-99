(* # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)

 type 'a rle =
 | One of 'a
 | Many of int * 'a

let encode l = 
  let rec aux l final_list (count, (el: string)) =
    match l with
    | [] -> (if count = 1 then (One el) else (Many (count, el))) :: final_list 
    | x :: xs -> 
      if x = el then
         aux xs final_list (count + 1, el)
      else
        aux xs (
          (if count = 1 then (One el) else Many (count, el))
          :: final_list) (1, x)
  in
  match l with
  | [] -> []
  | x :: xs -> List.rev (aux xs [] (1, x))
