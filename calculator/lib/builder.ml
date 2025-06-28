(** [parse s] parses [s] into an AST.  *)
let parse (s: string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val e] converts [e] to a string.
    Requires: [e] is a value.  *)
let string_of_val (e: Ast.expr) : string =
  match e with
  | Int i -> string_of_int i
  | Binop _ -> failwith "precondition violation"


(** [is_value e] is whether [e] is a value. *)
let is_value : Ast.expr -> bool = function
  | Int _ -> true
  | Binop _ -> false

(** [step e] takes a single step of evaluation  of [e].  *)
let rec step : Ast.expr -> Ast.expr = function
  | Int _ -> failwith "Int should not be a step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop(bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop(bop, step e1, e2)

and step_bop bop v1 v2 = match bop, v1, v2 with
  | Ast.Add, Ast.Int a, Ast.Int b -> Ast.Int (a + b)
  | Ast.Mult, Ast.Int a, Ast.Int b -> Ast.Int (a * b)
  | _ -> failwith "precondition violated"

(** [eval e] fully evals [e] to a value [v] *)
let rec eval (e: Ast.expr) : Ast.expr =
  if is_value e then e else 
    e |> step |> eval

let interp (s : string) : string =
  s |> parse |> eval |> string_of_val
