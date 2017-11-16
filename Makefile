build:
	-rm -r _build/*
	ocamlbuild -r main.ml tac.ml ssa.ml main.native -I src
	mv main.native main

graph:
	# dot -Tpng flow.dot -o flow.png
	dot -Tpdf flow.dot -o flow.pdf


run:
	make build
	./main
	make graph

clean:
	rm ./src/*~
