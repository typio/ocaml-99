let compress l =
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | x :: (y :: _ as xs) ->
      if x = y then 
        aux xs acc
      else 
        aux xs (x :: acc)
  in
  aux l []