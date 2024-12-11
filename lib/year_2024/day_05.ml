module Int_set = Set.Make (Int)
module Rules = Map.Make (Int)

let append_rule (before, after) rules =
  let append = function
    | Some afters -> Some (Int_set.add after afters)
    | None -> Some (Int_set.singleton after)
  in
  Rules.update before append rules
;;

let parse_rule rules line =
  let rule =
    match String.split_on_char '|' line with
    | [ left; right ] -> int_of_string left, int_of_string right
    | _ -> raise Parse.Invalid_input
  in
  append_rule rule rules
;;

let parse_updates line =
  let pages = line |> String.split_on_char ',' |> List.map int_of_string in
  let length = List.length pages in
  match length mod 2 with
  | 1 -> pages, List.nth pages (length / 2)
  | _ -> raise Parse.Invalid_input
;;

let parse input =
  let fold (is_rules, rules, updates) line =
    if line = ""
    then false, rules, updates
    else if is_rules
    then is_rules, parse_rule rules line, updates
    else is_rules, rules, List.cons (parse_updates line) updates
  in
  let _, rules, updates =
    input |> Reader.lines_of |> Seq.fold_left fold (true, Rules.empty, [])
  in
  rules, updates
;;

let rec validate rules pages =
  let aux rules before after =
    match Rules.find_opt before rules with
    | Some rules -> List.for_all (fun it -> Int_set.mem it rules) after
    | None -> false
  in
  match pages with
  | [] | _ :: [] -> true
  | before :: after -> if aux rules before after then validate rules after else false
;;

let sum_valid rules updates =
  let aux sum (updates, middle) = if validate rules updates then sum + middle else sum in
  updates |> List.fold_left aux 0
;;

let reorder rules updates =
  let compare x y =
    match Rules.find_opt x rules with
    | Some rules -> if Int_set.mem y rules then 1 else -1
    | None -> -1
  in
  List.sort compare updates
;;

let sum_invalid rules updates =
  let invalid =
    List.filter_map
      (fun (it, _) -> if validate rules it then None else Some (reorder rules it))
      updates
  in
  let middle xs =
    let index = List.length xs / 2 in
    List.nth xs index
  in
  invalid |> List.fold_left (fun sum it -> sum + middle it) 0
;;

let part_one input =
  let rules, updates = parse input in
  sum_valid rules updates |> string_of_int
;;

let part_two input =
  let rules, updates = parse input in
  sum_invalid rules updates |> string_of_int
;;
