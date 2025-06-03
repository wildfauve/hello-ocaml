module type QueueSig = sig
  type 'a queue

  val empty : 'a queue

  val enqueue : 'a -> 'a queue -> 'a queue
  
  val peek : 'a queue -> 'a option

  val dequeue : 'a queue -> 'a queue option
end

module Queue : QueueSig = struct
  type 'a queue = 'a list
  let empty = []

  let enqueue x q = q @ [x] (* That is a linear time op *)

  let peek = function
  | [] -> None
  | x :: _ -> Some x

  let dequeue = function
  | [] -> None
  | _ :: q -> Some q
end

module TwoListQueueImpl = struct
  (* Trying to avoid that @ (concat) op which has linear time *)
  (* The front is in order, while the back is in reverse order
    {front = [a;b]; back = [e;d;c]} 
    for the queue [a;b;c;d;e]
    If the front is empty then the back must also be empty to guarantee that the first element of
    the queue is always the head of front*)
  type 'a queue = {
    front : 'a list;
    back : 'a list;
  }

  let empty = {front = []; back = []}

  let peek = function
  | {front = []} -> None
  | {front = x :: _} -> Some x

  let enqueue x = function
  | {front = []} -> {front = [x]; back = []}
  | q -> {q with back = x :: q.back} (* which is a constant time op *)

  let dequeue = function
  | {front = []} -> None
  | {front = _ :: []; back} -> Some {front = List.rev back; back = []}
  | {front = _:: t; back} -> Some {front = t; back}

end

(* Here we bind the implementation of TwoListQueueImpl, which has the QueueSig to the name TwoListQueue 
  but it also means that TwoListQueue has the sig limitations, but TwoListQueueImpl does not.*)
module TwoListQueue : QueueSig = TwoListQueueImpl

(* Pipelining options *)
(* Queue.(empty |> enqueue 42 |> dequeue |> enqueue 43)  --- this wont compile as dequeue returns a Some queue*)
(* So, lets implement a version of Option.map 
 That is we create a new pipeline op that deals with the fact that dequeue returns an Option[queue]*)

 (* The |> op looks like this.., changing it to ||> so as not to clash *)
let ( ||> ) x f =
  f x

(* Dealing with the option.  This is the same as Option.map in the std lib
 https://ocaml.org/docs/options 
 Note that there is a problem here as dequeue returns a Some queue, so applying map here
 will return Some Some queue*)
let ( >>| ) opt f =
  match opt with 
  | None -> None          (* Supports keep passing None through when 1 opt in the pipeline is None *)
  | Some x -> Some (f x)  (* Returns the result of the f x wrapped in Some *)

(* Which gives us this sort of behaviour *)
let option_x = Some 10 >>| (fun x -> x * 2) >>| (fun x -> x + 1)


let option_q = Queue.(empty |> enqueue 42 |> dequeue >>| enqueue 43)

(* Now lets add a dequeue.  We cant just use the pipeline op ( |> ) as we have a Some queue
  We can use the option map op, but that will return Some Some queue!
  So want we want is an op like >>| but that instead of wrapping the result in an option
  it just returns the result of f x *)

let ( >>= ) opt f =
match opt with 
| None -> None
| Some x -> f x

(* And this is the bind fn (Option.bind, in fact) *)

let option_q' = Queue.(empty |> enqueue 42 |> dequeue >>| enqueue 43 >>= dequeue)

(* We can reformat for readability *)
let option_q'' = 
  let open Queue in
  empty 
  |> enqueue 42 
  |> dequeue 
  >>| enqueue 43 
  >>= dequeue

