(* Helper function. *)
let print_list to_string l =
  let rec loop rem acc =
      match rem with
      | [] -> acc
      | [s] -> acc ^ (to_string s)
      | (s::ss) ->
          loop ss (acc ^ (to_string s) ^ "; ") in
  print_string "[";
  print_string (loop l "");
  print_endline "]"

let () = 
(* We get a compile error when trying to apply empty |> insert 1 2 to List.map.  While the 
  implementation of our map is an association list, the type sig isn't 
  rather it is (int, int) t and List.map expects ('a * 'b) list. 
  Hence we call bindings to return a typed association list, cause bindings type is
  ('k, 'v) t -> ('k * 'v) list *)
  let open Makingmap.Map.AssocListMap in
  empty |> insert 1 2 |> bindings |> List.map fst |> print_list string_of_int
  
  (* |> List.map fst |> print_list string_of_int  *)
