let is_palindrome a =
  let rev l =
    let rec aux list reversed =
      match list with
      | [] -> reversed
      | x :: xs -> aux xs (x :: reversed)
    in
    aux l []
  in a = (rev a)