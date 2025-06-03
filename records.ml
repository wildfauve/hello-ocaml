type cat = {
  name: string;
  yob: int;
}

let dinsdale = {name= "dinsdale"; yob=1990}

(* Copy from an existing record *)
let doug = {dinsdale with name = "doug"}