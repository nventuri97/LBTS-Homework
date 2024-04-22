(*
    Return true if l1 is sublist of l2
*)
let rec sublist l1 l2 =
  match l1 with
  | [] -> true
  | e :: l -> if List.mem e l2 then sublist l l2 else false
