clean:
    rm -rf _build

build MODULE:
    ocamlbuild {{MODULE}}.cmo {{MODULE}}.cmi
