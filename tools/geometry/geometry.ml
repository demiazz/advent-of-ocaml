module Int_point_2d = struct
  type t = int * int

  let compare = Stdlib.compare
  let map f (x, y) = (f x, f y)
  let ( + ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let ( - ) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
end
