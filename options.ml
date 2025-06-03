let unwrap default opt = match opt with
| None -> default
| Some x -> x

(* Using Option *)
(*  Note its a good idea to use begin and end around a nested match statement. They are the same as using (...)*)
let rec list_max (lst : 'a list) : 'a option = 
  match lst with
  | [] -> None
  | h :: t -> begin
      match list_max t with 
      | None -> Some h
      | Some m -> Some (max h m)
    end

let x = list_max [1;2;3]
let y = list_max []

(* Using map *)
let m1 = Option.map (fun x -> x + 1) (Some 3)
let m2 = Option.map (fun x -> x + 1) None
