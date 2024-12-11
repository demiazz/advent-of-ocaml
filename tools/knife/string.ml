type t = string

let indices s = Utils.indices_of Stdlib.String.length s

let fold_lefti ~f ~initial s =
  let aux (acc, index) it = f acc index it, index + 1 in
  let result, _ = Stdlib.String.fold_left aux (initial, 0) s in
  result
;;
