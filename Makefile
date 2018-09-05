.PHONY: clean all main

OCB_OPT=-Is src -use-ocamlfind

all: main

main:
	ocamlbuild $(OCB_OPT) main.native

clean:
	ocamlbuild -clean
