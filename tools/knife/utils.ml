let bool_of_option = function
  | Some _ -> true
  | None -> false
;;

let indices_of length_of xs =
  let open Stdlib.Seq in
  ints 0 |> take (length_of xs)
;;

let index index _ = index
