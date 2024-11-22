let test_case_of (year, day, part, expected) =
  let name = Printf.sprintf "%0.4d.%0.2d.%0.2d" year day part in
  let file_name = Printf.sprintf "resources/%0.4d_%0.2d.txt" year day in
  let solve ic =
    ic |> Reader.from_in_channel |> Solver.solve ~year ~day ~part
  in
  let handle () = In_channel.with_open_text file_name solve in
  let run () = Alcotest.(check string) "are equal" expected (handle ()) in
  (name, [ Alcotest.test_case "OK" `Quick run ])

let answers = [ (2015, 1, 1, "232"); (2015, 1, 2, "1783") ]
let test_cases = answers |> List.map test_case_of

let () =
  let open Alcotest in
  run "Solver" test_cases
