module Key = struct
  type t = int * int * int

  let compare = Stdlib.compare
end

module RegistryMap = Map.Make (Key)
module IntSet = Set.Make (Stdlib.Int)

type 'a t = 'a RegistryMap.t

let empty () = RegistryMap.empty

exception Year_out_of_range of int
exception Day_out_of_range of int
exception Part_out_of_range of int

let validate_input year day part =
  let min_year = 2015 and max_year = 2024 in
  let min_day = 1 and max_day = 25 in
  let min_part = 1 and max_part = 2 in
  if year < min_year || year > max_year then raise (Year_out_of_range year)
  else if day < min_day || day > max_day then raise (Day_out_of_range year)
  else if part < min_part || part > max_part then raise (Part_out_of_range part)
  else (year, day, part)

exception Duplicated_entry of (int * int * int)

let add ~year ~day ~part ~value map =
  let key = validate_input year day part in
  match RegistryMap.mem key map with
  | true -> raise (Duplicated_entry key)
  | false -> RegistryMap.add key value map

let all_years map =
  let get_year (year, _, _) = year in
  let put_year set year = IntSet.add year set in
  let pairs = RegistryMap.bindings map in
  let keys = List.map fst pairs in
  let years = List.map get_year keys in
  let unique_years = List.fold_left put_year IntSet.empty years in
  IntSet.to_list unique_years

let all_days ~year map =
  let filter_map = function
    | current_year, day, _ when current_year == year -> Some day
    | _ -> None
  in
  let put_day set day = IntSet.add day set in
  let pairs = RegistryMap.bindings map in
  let keys = List.map fst pairs in
  let all_days = List.filter_map filter_map keys in
  let unique_days = List.fold_left put_day IntSet.empty all_days in
  IntSet.to_list unique_days

let all_parts ~year ~day map =
  let one = RegistryMap.mem (year, day, 1) map in
  let two = RegistryMap.mem (year, day, 2) map in
  (one, two)

let find_opt ~year ~day ~part map = RegistryMap.find_opt (year, day, part) map
