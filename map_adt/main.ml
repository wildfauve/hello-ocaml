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
  Map.MyMap.empty |> List.cons (1,2) |> List.map fst |> print_list string_of_int 
