(* The 2 files, stacksmli.ml and stacksmli.mli are a compilation unit. 
    The .ml file does not have module definitions, this is defined automatically by the compiler.
    The name of the module becomes the capitalised name of the file name ---> Stacksmli
    
    We can load a compiled version into utop (remember #use is something like copy and paste code into utop)
    > ocamlbuild stacksmli.cmo stacksmli.cmi 
    > utop -I _build
    > #use "stacksmli.cmo";;
    > Stacksmli.empty;;*)

type 'a stack   (* this is an abstract type, it declares the type, but does not define it
as type 'a stack = 'a list would.
we could have declared it as type 'a t to show it was abstract.*)

val empty : 'a stack

val push : 'a -> 'a stack -> 'a stack

val peek : 'a stack -> 'a

val pop : 'a stack -> 'a stack
