let printf = Format.printf
let fprintf = Format.fprintf

(* Print a quoted string *)
(** [ppf] is a pretty print formatter, [s] is a string to be printed.   *)
let pp_string ppf s = fprintf ppf "%S" s

(* A wrapper for Format.pp_print_list.  pp_print_list requires a separator needs a full
  pretty printer itself, hence the wrapper. *)
let pp_print_list ~sep pp_item =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf sep) pp_item

let example = [
  [];
  ["one"; "two"; "three"];
  [
    "one"; "two"; "three"; "four"; "five";
    "six"; "seven"; "eight"; "nine"; "ten";
  ];
]

let pp_list pp_item ppf list =
  fprintf ppf "@[<hv>[%a]@]"
    (pp_print_list ~sep:",@;<1 1>" pp_item) list


(* 
printf "%a" (pp_list (pp_list pp_string)) example
 *)