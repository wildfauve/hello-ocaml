open OUnit2
open Makingmap
 
let cmp_set_like_lists lst1 lst2 = 
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let bindings_test name expected input =
  name >:: fun _ -> 
    assert_equal expected (Map.AssocListMap.bindings input)
      ~cmp:cmp_set_like_lists

let option_test name expected input =
  name >:: fun _ ->
    assert_equal expected input 


let lst_with_2 = [("name", "Larry"); ("parent", "oscar")]

let assoc_lists = let open Map.AssocListMap in [
  bindings_test "empty has no bindings" [] empty;

  (let lst = [("name", "Larry")] in
  bindings_test "singleton list has 1 binding" lst (of_list lst));

  bindings_test "singleton list has 2 bindings" lst_with_2 (of_list lst_with_2);

  bindings_test "Insert a (k, v)" [(1, 2)] (empty |> insert 1 2);

  option_test "Finds an element successfully" (Some "oscar")  (find "parent" (of_list lst_with_2));

  bindings_test "Removes a (k, v)" [("name", "Larry")] (remove "parent" (of_list lst_with_2));
]

(* I wonder why I dont have to specify Map._ before using Hashtablemap.  I think its because I used 
  ml/mli files to represent the modules, where as Map.AssocListMap uses the module keyword.   *)
(* let hash_table_map = let open Hashtablemap in [] *)

(* let set_ = let open Setmap.ListSet in [] *)

let map_suite = "Map ADT Suite" >::: assoc_lists
(* let hash_suite = "Hash Table Suite" >::: hash_table_map *)

let _ = run_test_tt_main map_suite
(* let _ = run_test_tt_main hash_suite *)