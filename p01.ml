let rec last l =
  match l with
  | [] -> None
  | [n] -> Some n
  | h :: t -> last t
;;