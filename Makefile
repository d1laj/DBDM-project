all: 
	ocamlbuild -tag thread -yaccflag -v -lib unix main.native; mv main.native DEP

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
