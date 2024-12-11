type 'a t = 'a Stdlib.Seq.t

let exists ~f = Stdlib.Seq.exists f

let indices s = Stdlib.Seq.mapi Utils.index s

let sum = Stdlib.Seq.fold_left ( + ) 0

let fold_lefti ~f ~initial s =
  let aux (acc, index) it = f acc index it, index + 1 in
  let result, _ = Stdlib.Seq.fold_left aux (initial, 0) s in
  result
;;

let pairs_of s =
  let open Stdlib.Seq in
  zip s (drop 1 s)
;;

let triples_of s =
  let open Stdlib.Seq in
  let triple_of (x, y) z = x, y, z in
  map2 triple_of (pairs_of s) (drop 2 s)
;;
