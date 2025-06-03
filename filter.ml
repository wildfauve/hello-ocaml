let rec evens = function
| [] -> []
| h :: t -> begin
  if h mod 2 = 0
    then h :: evens t
  else evens t
end

let rec odds = function
| [] -> []
| h :: t -> begin
  if h mod 2 = 1
    then h :: odds t
  else odds t
end

(* Again refactor ... so we'll get to List.filter *)
(* Note that this function is not tail recursive, as there is work to be done at the end of the 
  recursion; which is the h :: () part. 
  look back at the filter video https://www.youtube.com/watch?v=FaWtD-LRdpU&list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU&index=51 *)

let rec filter predicate = function
  | [] -> []
  | h :: t -> if predicate h then h :: filter predicate t else filter predicate t

let is_odd x = x mod 2 = 1
let is_even x = x mod 2 = 0