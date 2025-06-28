{
    open Parser
}

let digit = ['0'-'9']
let int = '-'? digit+
let sp = [' ' '\t']+

rule read =
    parse 
    | sp { read lexbuf }
    | "+" { PLUS }
    | "*" { MULT }
    | "(" { LPEREN }
    | ")" { RPEREN }
    | int { INT (int_of_string(Lexing.lexeme lexbuf)) }
    | eof { EOF }