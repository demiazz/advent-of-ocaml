type 'a t = 'a list

let indices xs = Utils.indices_of Stdlib.List.length xs

let remove_at ~index =
  let remove it _ = it <> index in
  Stdlib.List.filteri remove
;;
