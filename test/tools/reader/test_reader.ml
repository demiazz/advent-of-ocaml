module type Input = sig
  val kind : string

  val from_input : string -> (Reader.t -> 'a) -> 'a
end

module String_input = struct
  let kind = "string"

  let from_input input work = input |> Reader.from_string |> work
end

module File_input = struct
  let kind = "file"

  let remove file_name =
    try Sys.remove file_name with
    | _ -> ()
  ;;

  let write file_name data =
    let handle oc =
      Out_channel.output_string oc data;
      Out_channel.flush oc
    in
    Out_channel.with_open_text file_name handle
  ;;

  let read file_name work =
    let handle ic = ic |> Reader.from_in_channel |> work in
    In_channel.with_open_text file_name handle
  ;;

  let from_input input work =
    let file_name = Filename.temp_file "reader" ".tmp" in
    let _ = print_endline file_name in
    let _ = Out_channel.flush_all in
    try
      write file_name input;
      let result = read file_name work in
      remove file_name;
      result
    with
    | exn ->
      remove file_name;
      raise exn
  ;;
end

module Suite (Input : Input) = struct
  let test_chars_of name (input, expected) =
    let handle reader = reader |> Reader.chars_of |> String.of_seq in
    let actual input = Input.from_input input handle in
    let run () = input |> actual |> Alcotest.(check string) "are equal" expected in
    Alcotest.test_case name `Quick run
  ;;

  let test_bytes_of name (input, expected) =
    let handle reader = reader |> Reader.bytes_of |> List.of_seq in
    let actual input = Input.from_input input handle in
    let run () = input |> actual |> Alcotest.(check (list int)) "are equal" expected in
    Alcotest.test_case name `Quick run
  ;;

  let test_lines_of name (input, expected) =
    let handle reader = reader |> Reader.lines_of |> List.of_seq in
    let actual input = Input.from_input input handle in
    let run () = input |> actual |> Alcotest.(check (list string)) "are equal" expected in
    Alcotest.test_case name `Quick run
  ;;

  let test_cases_of scope create samples =
    let suite_name = Printf.sprintf "%s :: %s" Input.kind scope in
    let test_case_of (name, input, expected) = create name (input, expected) in
    let test_cases = samples |> List.map test_case_of in
    suite_name, test_cases
  ;;

  let suite_chars_of =
    let samples =
      [ "when empty", "", ""
      ; "when starts with a new line", "\nrest", ""
      ; "when non empty", "hello", "hello"
      ; "when contains a new line", "hello\nworld", "hello"
      ]
    in
    test_cases_of "chars_of" test_chars_of samples
  ;;

  let suite_bytes_of =
    let samples =
      [ "when empty", "", []
      ; "when starts with a new line", "\nrest", []
      ; "when non empty", "hello", [ 104; 101; 108; 108; 111 ]
      ; "when contains a new line", "hello\nworld", [ 104; 101; 108; 108; 111 ]
      ]
    in
    test_cases_of "bytes_of" test_bytes_of samples
  ;;

  let suite_lines_of =
    let samples =
      [ "when empty", "", []
      ; "when contains only a new line", "\n", [ "" ]
      ; "when contains only new lines", "\n\n", [ ""; "" ]
      ; "when contains multiple lines", "hello\nworld", [ "hello"; "world" ]
      ; ( "when contains multiple lines and ends with a new line"
        , "hello\nworld\n"
        , [ "hello"; "world" ] )
      ; ( "when contains multiple lines and ends with multiple new lines"
        , "hello\nworld\n\n"
        , [ "hello"; "world"; "" ] )
      ; ( "when contains multiple new lines in middle of string"
        , "hello\n\n\nworld"
        , [ "hello"; ""; ""; "world" ] )
      ]
    in
    test_cases_of "lines_of" test_lines_of samples
  ;;
end

module String_suite = Suite (String_input)
module File_suite = Suite (File_input)

let () =
  let open Alcotest in
  run
    "Reader"
    [ String_suite.suite_chars_of
    ; String_suite.suite_bytes_of
    ; String_suite.suite_lines_of
    ; File_suite.suite_chars_of
    ; File_suite.suite_bytes_of
    ; File_suite.suite_lines_of
    ]
;;
