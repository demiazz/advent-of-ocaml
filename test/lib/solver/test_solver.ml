let test_case_of (year, day, part, expected) =
  let name = Printf.sprintf "%0.4d.%0.2d.%0.2d" year day part in
  let file_name = Printf.sprintf "resources/%0.4d_%0.2d.txt" year day in
  let solve ic = ic |> Reader.from_in_channel |> Solver.solve ~year ~day ~part in
  let handle () = In_channel.with_open_text file_name solve in
  let run () = Alcotest.(check string) "are equal" expected (handle ()) in
  name, [ Alcotest.test_case "OK" `Quick run ]
;;

let answers =
  [ 2015, 1, 1, "232"
  ; 2015, 1, 2, "1783"
  ; 2015, 2, 1, "1598415"
  ; 2015, 2, 2, "3812909"
  ; 2015, 3, 1, "2565"
  ; 2015, 3, 2, "2639"
  ; 2015, 4, 1, "254575"
  ; 2015, 4, 2, "1038736"
  ; 2015, 5, 1, "238"
  ; 2015, 5, 2, "69"
  ; 2024, 1, 1, "2192892"
  ; 2024, 1, 2, "22962826"
  ; 2024, 2, 1, "359"
  ; 2024, 2, 2, "418"
  ; 2024, 3, 1, "155955228"
  ; 2024, 3, 2, "100189366"
  ; 2024, 4, 1, "2336"
  ; 2024, 4, 2, "1831"
  ; 2024, 5, 1, "5268"
  ; 2024, 5, 2, "5799"
  ; 2024, 6, 1, "4819"
  ; 2024, 6, 2, "1796"
  ; 2024, 7, 1, "3312271365652"
  ; 2024, 7, 2, "509463489296712"
  ; 2024, 8, 1, "220"
  ; 2024, 8, 2, "813"
  ; 2024, 9, 1, "6310675819476"
  ; 2024, 9, 2, "6335972980679"
  ]
;;

let test_cases = answers |> List.map test_case_of

let () =
  let open Alcotest in
  run "Solver" test_cases
;;
