module Chars_of = struct
  let process_char char =
    match char with 'a' -> Some 0 | 'b' -> Some 1 | _ -> None

  let handle input =
    input |> Reader.from_string |> Parse.chars_of process_char |> List.of_seq

  let test_correct_input name input expected =
    let run () =
      Alcotest.(check (list int)) "are equal" expected (handle input)
    in
    Alcotest.test_case name `Quick run

  let test_incorrect_input name input =
    let run () =
      Alcotest.check_raises "unknown character raises exception"
        Parse.Invalid_input (fun () -> input |> handle |> ignore)
    in
    Alcotest.test_case name `Quick run

  let test_cases =
    [
      test_correct_input "when empty" "" [];
      test_correct_input "when non empty" "ababaabb" [ 0; 1; 0; 1; 0; 0; 1; 1 ];
      test_correct_input "when empty with a new line" "\nab" [];
      test_correct_input "when non empty with a new line" "ababaabb\nab"
        [ 0; 1; 0; 1; 0; 0; 1; 1 ];
      test_incorrect_input "when incorrect input" "abcabaabb";
    ]

  let suite = ("chars_of", test_cases)
end

let () =
  let open Alcotest in
  run "Parse" [ Chars_of.suite ]
