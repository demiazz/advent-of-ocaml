module Point = Point_2d.Make (Int)

let pair_t =
  let pp fmt point = Format.fprintf fmt "%s" (Point.to_string point) in
  Alcotest.testable pp Point.equal

let test_zero name =
  let run () =
    Alcotest.(check pair_t) "are equal" (Point.create 0 0) Point.zero
  in
  Alcotest.test_case name `Quick run

let test_create name x y =
  let run () =
    let actual = Point.create x y in
    Alcotest.(check pair_t) "are equal" { x; y } actual
  in
  Alcotest.test_case name `Quick run

let test_compare name left right expected =
  let run () =
    let actual = Point.compare left right in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_equal name left right expected =
  let run () =
    let actual = Point.equal left right in
    Alcotest.(check bool) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_min_x name left right expected =
  let run () =
    let actual = Point.min_x left right in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_min_y name left right expected =
  let run () =
    let actual = Point.min_y left right in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_max_x name left right expected =
  let run () =
    let actual = Point.max_x left right in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_max_y name left right expected =
  let run () =
    let actual = Point.max_y left right in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_x name initial expected =
  let run () =
    let actual = Point.x initial in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_y name initial expected =
  let run () =
    let actual = Point.y initial in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_add_value name initial value expected =
  let run () =
    let actual = Point.( + ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_sub_value name initial value expected =
  let run () =
    let actual = Point.( - ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_mul_value name initial value expected =
  let run () =
    let actual = Point.( * ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_div_value name initial value expected =
  let run () =
    let actual = Point.( / ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_add_points name left right expected =
  let run () =
    let actual = Point.( ++ ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_sub_points name left right expected =
  let run () =
    let actual = Point.( -- ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_mul_points name left right expected =
  let run () =
    let actual = Point.( ** ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_div_points name left right expected =
  let run () =
    let actual = Point.( // ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_of_pair name initial expected =
  let run () =
    let actual = Point.of_pair initial in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_to_pair name initial expected =
  let run () =
    let actual = Point.to_pair initial in
    Alcotest.(check (pair int int)) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_to_string name initial expected =
  let run () =
    let actual = Point.to_string initial in
    Alcotest.(check string) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let () =
  let open Alcotest in
  run "Point_2d"
    [
      ("zero      : t", [ test_zero "is point with zero coordinates" ]);
      ( "create    : c -> c -> t",
        [ test_create "creates a point with given coordinates" 4 2 ] );
      ( "compare   : t -> t -> int (x = x and y = y)",
        [
          test_compare "returns 0 when coordinates of both points are equal"
            { x = 4; y = 2 } { x = 4; y = 2 } 0;
        ] );
      ( "compare   : t -> t -> int (x > x and y < y)",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 2 } { x = 4; y = 2 } 1;
        ] );
      ( "compare   : t -> t -> int (x > x and y = y)",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 2 } { x = 4; y = 2 } 1;
        ] );
      ( "compare   : t -> t -> int (x > x and y > y)",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 3 } { x = 4; y = 2 } 1;
        ] );
      ( "compare   : t -> t -> int (x < x and y < y)",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 2 } { x = 4; y = 2 } (-1);
        ] );
      ( "compare   : t -> t -> int (x < x and y = y)",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 2 } { x = 4; y = 2 } (-1);
        ] );
      ( "compare   : t -> t -> int (x < x and y > y)",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 3 } { x = 4; y = 2 } (-1);
        ] );
      ( "compare   : t -> t -> int (x = x and y > y)",
        [
          test_compare
            "returns 1 when coordinates when y of first point is greater"
            { x = 4; y = 3 } { x = 4; y = 2 } 1;
        ] );
      ( "compare   : t -> t -> int (x = x and y < y)",
        [
          test_compare
            "returns -1 when coordinates when y of first point is lesser"
            { x = 3; y = 2 } { x = 4; y = 2 } (-1);
        ] );
      ( "equal     : t -> t -> bool (x = x and y = y)",
        [
          test_equal "returns true when coordinates of both points are equal"
            { x = 4; y = 2 } { x = 4; y = 2 } true;
        ] );
      ( "equal     : t -> t -> bool (x != x and y = y)",
        [
          test_equal
            "returns false when x coordinates of both points aren't equal"
            { x = 3; y = 3 } { x = 4; y = 2 } false;
        ] );
      ( "equal     : t -> t -> bool (x = x and y != y)",
        [
          test_equal
            "returns false when y coordinates of both points aren't equal"
            { x = 4; y = 3 } { x = 4; y = 2 } false;
        ] );
      ( "min_x : t -> t -> c (x = x)",
        [
          test_min_x "returns x coordinate when both are equal" { x = 4; y = 0 }
            { x = 4; y = 0 } 4;
        ] );
      ( "min_x : t -> t -> c (x < x)",
        [
          test_min_x "returns left x coordinate when left is lesser"
            { x = 3; y = 0 } { x = 4; y = 0 } 3;
        ] );
      ( "min_x : t -> t -> c (x > x)",
        [
          test_min_x "returns right x coordinate when right is lesser"
            { x = 4; y = 0 } { x = 3; y = 0 } 3;
        ] );
      ( "min_y : t -> t -> c (y = y)",
        [
          test_min_y "returns y coordinate when both are equal" { x = 0; y = 4 }
            { x = 0; y = 4 } 4;
        ] );
      ( "min_y : t -> t -> c (y < y)",
        [
          test_min_y "returns left y coordinate when left is lesser"
            { x = 0; y = 3 } { x = 0; y = 4 } 3;
        ] );
      ( "min_y : t -> t -> c (y > y)",
        [
          test_min_y "returns right y coordinate when right is lesser"
            { x = 0; y = 4 } { x = 0; y = 3 } 3;
        ] );
      ( "max_x : t -> t -> c (y = y)",
        [
          test_max_x "returns y coordinate when both are equal" { x = 4; y = 0 }
            { x = 4; y = 0 } 4;
        ] );
      ( "max_x : t -> t -> c (y < y)",
        [
          test_max_x "returns right y coordinate when left is lesser"
            { x = 3; y = 0 } { x = 4; y = 0 } 4;
        ] );
      ( "max_x : t -> t -> c (y > y)",
        [
          test_max_x "returns left y coordinate when right is lesser"
            { x = 4; y = 0 } { x = 3; y = 0 } 4;
        ] );
      ( "max_y : t -> t -> c (y = y)",
        [
          test_max_y "returns y coordinate when both are equal" { x = 0; y = 4 }
            { x = 0; y = 4 } 4;
        ] );
      ( "max_y : t -> t -> c (y < y)",
        [
          test_max_y "returns right y coordinate when left is lesser"
            { x = 0; y = 3 } { x = 0; y = 4 } 4;
        ] );
      ( "max_y : t -> t -> c (y > y)",
        [
          test_max_y "returns left y coordinate when right is lesser"
            { x = 0; y = 4 } { x = 0; y = 3 } 4;
        ] );
      ( "x         : t -> c",
        [ test_x "returns x coordinate of point" { x = 4; y = 2 } 4 ] );
      ( "y         : t -> c",
        [ test_y "returns y coordinate of point" { x = 4; y = 2 } 2 ] );
      ( "add       : t -> c -> t",
        [
          test_add_value "adds a value to both coordinates" { x = 2; y = 0 } 2
            { x = 4; y = 2 };
        ] );
      ( "sub       : t -> c -> t",
        [
          test_sub_value "adds a value to both coordinates" { x = 4; y = 2 } 2
            { x = 2; y = 0 };
        ] );
      ( "mul       : t -> c -> t",
        [
          test_mul_value "multiplies both coordinates on a value"
            { x = 4; y = 2 } 2 { x = 8; y = 4 };
        ] );
      ( "div       : t -> c -> t",
        [
          test_div_value "divides both coordinates on a value" { x = 4; y = 2 }
            2 { x = 2; y = 1 };
        ] );
      ( "add       : t -> t -> t",
        [
          test_add_points "adds a value to both coordinates" { x = 2; y = 0 }
            { x = -4; y = 2 } { x = -2; y = 2 };
        ] );
      ( "sub       : t -> t -> t",
        [
          test_sub_points "substracts a value to both coordinates"
            { x = 2; y = 0 } { x = -2; y = -2 } { x = 4; y = 2 };
        ] );
      ( "mul       : t -> t -> t",
        [
          test_mul_points "multiplies a value to both coordinates"
            { x = 2; y = 4 } { x = -4; y = 2 } { x = -8; y = 8 };
        ] );
      ( "div       : t -> t -> t",
        [
          test_div_points "divides a value to both coordinates" { x = 8; y = 4 }
            { x = -4; y = 2 } { x = -2; y = 2 };
        ] );
      ( "of_pair   : (c * c) -> t",
        [
          test_of_pair "creates point from pair of coordinates" (8, 4)
            { x = 8; y = 4 };
        ] );
      ( "to_pair   : t -> (c * c)",
        [
          test_to_pair "creates point from pair of coordinates" { x = 8; y = 4 }
            (8, 4);
        ] );
      ( "to_string : t -> string",
        [
          test_to_string "creates string representation of point"
            { x = 8; y = -4 } "{ x = 8; y = -4 }";
        ] );
    ]
