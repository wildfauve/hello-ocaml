type 'a stack =
| Empty
| Entry of 'a * 'a stack

let empty = Empty

let push x s = Entry (x, s)

let peek = function
| Empty -> failwith "Its Empty!"
| Entry (x, _) -> x

let pop = function
| Empty -> failwith "Its Empty!"
| Entry (_, s) -> s
