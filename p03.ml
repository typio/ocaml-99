let rec nth l n =
  match l, n with
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: xs, _ -> nth xs (n-1)
