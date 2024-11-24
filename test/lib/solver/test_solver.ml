let test_case_of (year, day, part, expected) =
  let name = Printf.sprintf "%0.4d.%0.2d.%0.2d" year day part in
  let file_name = Printf.sprintf "resources/%0.4d_%0.2d.txt" year day in
  let solve ic =
    ic |> Reader.from_in_channel |> Solver.solve ~year ~day ~part
  in
  let handle () = In_channel.with_open_text file_name solve in
  let run () = Alcotest.(check string) "are equal" expected (handle ()) in
  (name, [ Alcotest.test_case "OK" `Quick run ])

(* let available =
   let days_of year =
     let to_tuple day = (year, day) in
     Solver.days ~year |> List.map to_tuple
   in
   let parts_of (year, day) =
     let parts = Solver.parts ~year ~day in
     (year, day, parts)
   in
   let keys_of (year, day, parts) =
     match parts with
     | true, true -> [ (year, day, 1); (year, day, 2) ]
     | true, false -> [ (year, day, 1) ]
     | false, true -> [ (year, day, 2) ]
     | false, false -> []
   in
   Solver.years |> List.map days_of |> List.flatten |> List.map parts_of
   |> List.map keys_of |> List.flatten *)

let answers = [ (2015, 1, 1, "232"); (2015, 1, 2, "1783") ]
let test_cases = answers |> List.map test_case_of

let () =
  let open Alcotest in
  run "Solver" test_cases
