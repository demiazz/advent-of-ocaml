let usage = "advent-of-ocaml -y <year> -d <day> -p <part> -i <file>"

let year = ref 0

let day = ref 0

let part = ref 0

let input_file = ref ""

let speclist =
  [ "-y", Arg.Set_int year, "Set year"
  ; "-d", Arg.Set_int day, "Set day"
  ; "-p", Arg.Set_int part, "Set part"
  ; "-i", Arg.Set_string input_file, "Set input file name"
  ]
;;

let () =
  Arg.parse speclist (fun _ -> ()) usage;
  let solve ic =
    try
      let input = Reader.from_in_channel ic in
      let start = Sys.time () in
      let answer = Solver.solve ~year:!year ~day:!day ~part:!part input in
      let finish = Sys.time () in
      Ok (answer, finish -. start)
    with
    | exn -> Error (Printexc.to_string exn)
  in
  let result = In_channel.with_open_text input_file.contents solve in
  match result with
  | Ok (message, time) ->
    Printf.fprintf stdout "Answer: %s (%.1fms)\n" message time;
    exit 0
  | Error message ->
    Printf.fprintf stderr "Error: %s\n" message;
    exit 1
;;
