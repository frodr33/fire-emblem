compile:
	ocamlc -c command.mli
	ocamlc -c types.mli
	ocamlc -c state.mli
	ocamlc -c character.mli
	ocamlc -c sprites.mli
	ocamlopt -c types.ml
	ocamlopt -c state.ml
