## Build and Run Tests

```shell
dune build
opam exec -- dune test
```

## Running the App

Its called `makingmaps` because the public name of the app is `makingmaps`.  From the main `dune` file

```ocaml
(executable
 (name main)
 (public_name makingmaps)
 (libraries ounit2 map))
```


```shell
opam exec -- dune exec makingmaps
```