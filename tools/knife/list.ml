type 'a t = 'a list

let indices xs = Utils.indices_of Stdlib.List.length xs

let rec find_first_opt ~f xs =
  match xs with
  | x :: xs -> if f x then Some x else find_first_opt ~f xs
  | [] -> None
;;

let find_last_opt ~f =
  let aux result x = if f x then Some x else result in
  Stdlib.List.fold_left aux None
;;

let remove_at ~index =
  let remove it _ = it <> index in
  Stdlib.List.filteri remove
;;
