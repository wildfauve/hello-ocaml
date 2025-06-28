open OUnit2
open Calc

let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_int i) (Builder.interp s))

let tests = [
  make_i "int" 22 "22";
  make_i "add" 22 "11+11";
  make_i "multiply" 22 "11*2";
  make_i "multiply" 40 "2*2*10";
  make_i "add and multiply" 22 "2+2*10";
  make_i "multiply and add" 14 "2*2+10";
  make_i "multiply and ass with precidence" 14 "2*2+10";
  make_i "nested with white space" 22 "(10 + 1) + (5 + 6)";
]

let _ = run_test_tt_main ("suite" >::: tests)