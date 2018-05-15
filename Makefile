build:
	ocamlbuild -use-ocamlfind \
		-plugin-tag "package(js_of_ocaml.ocamlbuild)" \
		-no-links \
		main.d.js
	ocamlbuild -use-ocamlfind types.cmo charactermaker.cmo characters.cmo interactions.cmo room.cmo state.cmo ai.cmo command.cmo gui.cmo -r

clean:
	ocamlbuild -clean

test:
	ocamlbuild -use-ocamlfind test2.byte && ./test2.byte
# compile:
# 	ocamlc -c types.mli
# 	ocamlc -c room.mli
# 	ocamlc -c state.mli
# 	ocamlc -c characters.mli
# 	ocamlc -c items.mli
# 	ocamlc -c sprites.mli
# 	ocamlc -c command.mli
