type token =
  | INT of (
# 1 "parser.mly"
        int
# 6 "parser.mli"
)
  | PLUS
  | MULT
  | LPEREN
  | RPEREN
  | EOF

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
