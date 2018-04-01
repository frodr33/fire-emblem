compile:
	ocamlc -c types.mli
	ocamlopt -c types.ml
	ocamlc -c state.mli
	ocamlopt -c state.ml
