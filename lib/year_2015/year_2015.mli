module type Day = sig
  val part_one : Reader.t -> string
  val part_two : Reader.t -> string
end

module Day_01 : Day
