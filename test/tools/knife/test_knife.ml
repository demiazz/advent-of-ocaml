module List_suite = struct
  let test_indices name xs expected =
    let run () =
      Alcotest.(check (list int))
        "are equal"
        expected
        (xs |> Knife.List.indices |> List.of_seq)
    in
    Alcotest.test_case name `Quick run
  ;;

  let indices_suite =
    ( "List.indices"
    , [ test_indices "empty list" [] []
      ; test_indices "non empty list" [ 1; 2; 3 ] [ 0; 1; 2 ]
      ] )
  ;;

  let test_find_first_opt name xs expected =
    let is_match x = x > 3 in
    let run () =
      Alcotest.(check (option int))
        "are equal"
        expected
        (Knife.List.find_first_opt ~f:is_match xs)
    in
    Alcotest.test_case name `Quick run
  ;;

  let find_first_opt_suite =
    ( "List.find_first_opt"
    , [ test_find_first_opt "empty list" [] None
      ; test_find_first_opt "non empty list without match" [ 1; 2; 3 ] None
      ; test_find_first_opt
          "non empty list with match"
          [ 1; 2; 3; 4; 1; 5; 2; 6 ]
          (Some 4)
      ] )
  ;;

  let test_find_last_opt name xs expected =
    let is_match x = x > 3 in
    let run () =
      Alcotest.(check (option int))
        "are equal"
        expected
        (Knife.List.find_last_opt ~f:is_match xs)
    in
    Alcotest.test_case name `Quick run
  ;;

  let find_last_opt_suite =
    ( "List.find_last_opt"
    , [ test_find_last_opt "empty list" [] None
      ; test_find_last_opt "non empty list without match" [ 1; 2; 3 ] None
      ; test_find_last_opt "non empty list with match" [ 1; 2; 3; 4; 1; 5; 2; 6 ] (Some 6)
      ] )
  ;;

  let test_remove_at name index xs expected =
    let run () =
      Alcotest.(check (list int)) "are equal" expected (Knife.List.remove_at ~index xs)
    in
    Alcotest.test_case name `Quick run
  ;;

  let remove_at_suite =
    ( "List.remove_at"
    , [ test_remove_at "empty list" 10 [] []
      ; test_remove_at "out of range" 10 [ 1; 2; 3 ] [ 1; 2; 3 ]
      ; test_remove_at "negative index" (-1) [ 1; 2; 3 ] [ 1; 2; 3 ]
      ; test_remove_at "in range" 3 [ 0; 10; 20; 30; 40; 50 ] [ 0; 10; 20; 40; 50 ]
      ] )
  ;;

  let suites =
    [ indices_suite; find_first_opt_suite; find_last_opt_suite; remove_at_suite ]
  ;;
end

module Seq_suite = struct
  let test_exists name f s expected =
    let run () = Alcotest.(check bool) "are equal" expected (Knife.Seq.exists ~f s) in
    Alcotest.test_case name `Quick run
  ;;

  let exists_suite =
    let s = Seq.ints 0 |> Seq.take 5 in
    ( "Seq.exists"
    , [ test_exists "has no matched element" (Stdlib.( = ) 10) s false
      ; test_exists "has matched element" (Stdlib.( = ) 3) s true
      ; test_exists "has matched elements" (Stdlib.( > ) 3) s true
      ] )
  ;;

  let test_indices name s expected =
    let run () =
      Alcotest.(check (list int))
        "are equal"
        expected
        (s |> Knife.Seq.indices |> List.of_seq)
    in
    Alcotest.test_case name `Quick run
  ;;

  let indices_suite =
    ( "Seq.indices"
    , [ test_indices "empty sequence" Seq.empty []
      ; test_indices "non empty sequence" (Seq.ints 0 |> Seq.take 3) [ 0; 1; 2 ]
      ] )
  ;;

  let test_sum name s expected =
    let run () = Alcotest.(check int) "are equal" expected (Knife.Seq.sum s) in
    Alcotest.test_case name `Quick run
  ;;

  let sum_suite =
    ( "Seq.sum"
    , [ test_sum "empty sequence" Seq.empty 0
      ; test_sum "non empty sequence" (Seq.ints 0 |> Seq.take 4) 6
      ] )
  ;;

  let test_fold_lefti name s expected =
    let run () =
      let actual =
        Knife.Seq.fold_lefti
          ~f:(fun acc index it -> List.cons (index, it) acc)
          ~initial:[]
          s
      in
      Alcotest.(check (list (pair int int))) "are equal" expected actual
    in
    Alcotest.test_case name `Quick run
  ;;

  let fold_lefti_suite =
    ( "Seq.fold_lefti"
    , [ test_fold_lefti "empty sequence" Seq.empty []
      ; test_fold_lefti
          "non empty sequence"
          (List.to_seq [ 1; 2; 3 ])
          [ 2, 3; 1, 2; 0, 1 ]
      ] )
  ;;

  let test_pairs_of name s expected =
    let run () =
      Alcotest.(check (list (pair int int)))
        "are equal"
        expected
        (s |> Knife.Seq.pairs_of |> List.of_seq)
    in
    Alcotest.test_case name `Quick run
  ;;

  let pairs_of_suite =
    ( "Seq.pairs_of"
    , [ test_pairs_of "empty sequence" Seq.empty []
      ; test_pairs_of "single item sequence" (Seq.return 1) []
      ; test_pairs_of
          "multiple items sequence"
          (Seq.ints 0 |> Seq.take 5)
          [ 0, 1; 1, 2; 2, 3; 3, 4 ]
      ] )
  ;;

  let test_triples_of name s expected =
    let run () =
      Alcotest.(check (list (triple int int int)))
        "are equal"
        expected
        (s |> Knife.Seq.triples_of |> List.of_seq)
    in
    Alcotest.test_case name `Quick run
  ;;

  let triples_of_suite =
    ( "Seq.triples_of"
    , [ test_triples_of "empty sequence" Seq.empty []
      ; test_triples_of "single item sequence" (Seq.return 1) []
      ; test_triples_of "double items sequence" (Seq.ints 0 |> Seq.take 2) []
      ; test_triples_of
          "multiple items sequence"
          (Seq.ints 0 |> Seq.take 5)
          [ 0, 1, 2; 1, 2, 3; 2, 3, 4 ]
      ] )
  ;;

  let suites =
    [ exists_suite
    ; indices_suite
    ; sum_suite
    ; fold_lefti_suite
    ; pairs_of_suite
    ; triples_of_suite
    ]
  ;;
end

module String_suite = struct
  let test_indices name xs expected =
    let run () =
      Alcotest.(check (list int))
        "are equal"
        expected
        (xs |> Knife.String.indices |> List.of_seq)
    in
    Alcotest.test_case name `Quick run
  ;;

  let indices_suite =
    ( "String.indices"
    , [ test_indices "empty string" "" []
      ; test_indices "non empty string" "abc" [ 0; 1; 2 ]
      ] )
  ;;

  let test_fold_lefti name s expected =
    let run () =
      let actual =
        Knife.String.fold_lefti
          ~f:(fun acc index char -> List.cons (index, char) acc)
          ~initial:[]
          s
      in
      Alcotest.(check (list (pair int char))) "are equal" expected actual
    in
    Alcotest.test_case name `Quick run
  ;;

  let fold_lefti_suite =
    ( "String.fold_lefti"
    , [ test_fold_lefti "empty string" "" []
      ; test_fold_lefti "non empty string" "abc" [ 2, 'c'; 1, 'b'; 0, 'a' ]
      ] )
  ;;

  let suites = [ indices_suite; fold_lefti_suite ]
end

let () =
  let open Alcotest in
  run "Knife" (List_suite.suites @ Seq_suite.suites @ String_suite.suites)
;;
