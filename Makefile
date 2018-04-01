compile:
	ocamlc -c command.mli
	ocamlc -c types.mli
	ocamlc -c state.mli
	ocamlopt -c types.ml
	ocamlopt -c state.ml
