module Int_point_2d = struct
  open Geometry

  let test_compare name left right expected =
    let run () =
      let result = Int_point_2d.compare left right in
      Alcotest.(check int) "are equal" expected result
    in
    Alcotest.test_case name `Quick run

  let compare_suite =
    ( "Int_point_2d.compare",
      [
        test_compare "are equals if x1 = x2 && y1 = y2" (0, 0) (0, 0) 0;
        test_compare "left greater if x1 > x2 && y1 = y2" (1, 0) (0, 0) 1;
        test_compare "left greater if x1 > x2 && y1 < y2" (1, 0) (0, 0) 1;
        test_compare "left smaller if x1 < x2 && y1 = y2" (0, 0) (1, 0) (-1);
        test_compare "left smaller if x1 < x2 && y1 > y2" (1, 0) (0, 0) 1;
        test_compare "right greater if x1 = x2 && y1 > y2" (1, 0) (0, 0) 1;
        test_compare "right smaller if x1 = x2 && y1 < y2" (0, 0) (1, 0) (-1);
      ] )

  let test_map name target expected =
    let run () =
      let result = Int_point_2d.map string_of_int target in
      Alcotest.(check (pair string string)) "are equal" expected result
    in
    Alcotest.test_case name `Quick run

  let map_suite =
    ("Int_point_2d.map", [ test_map "string_of_int" (2, 4) ("2", "4") ])

  let test_plus name left right expected =
    let run () =
      let open Int_point_2d in
      let result = left + right in
      Alcotest.(check (pair int int)) "are equal" expected result
    in
    Alcotest.test_case name `Quick run

  let plus_suite =
    ("Int_point_2d.(+)", [ test_plus "+" (1, 2) (-2, -1) (-1, 1) ])

  let test_minus name left right expected =
    let run () =
      let open Int_point_2d in
      let result = left - right in
      Alcotest.(check (pair int int)) "are equal" expected result
    in
    Alcotest.test_case name `Quick run

  let minus_suite =
    ("Int_point_2d.(-)", [ test_minus "-" (1, 2) (2, 1) (-1, 1) ])
end

let () =
  let open Alcotest in
  run "Geometry"
    [
      Int_point_2d.compare_suite;
      Int_point_2d.map_suite;
      Int_point_2d.plus_suite;
      Int_point_2d.minus_suite;
    ]
