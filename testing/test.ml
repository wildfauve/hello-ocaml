open OUnit2
open Leap_year

let leap_year_test (n, y, b) =
  n >:: fun _ -> assert_equal b (leap_year y)

let tests = List.map leap_year_test [
  "not a leap year", 2010, false;
  "not centennial", 2020, true;
  "quadracentennial", 2000, true;
]

let suite = "leap year" >::: tests

let _  = run_test_tt_main suite