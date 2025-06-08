open OUnit2
open Map
 
let assoc_lists = let open AssocListMap in [
  "empty has no bindings" >:: (fun _ -> assert_equal [] (bindings empty));
]

let suite = "Map ADT Suite" >::: assoc_lists

let _ = run_test_tt_main suite