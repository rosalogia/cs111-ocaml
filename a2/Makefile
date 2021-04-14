OCAMLC=ocamlopt
OUTPUTS=largest_of_five check_digit random_walker

all: $(OUTPUTS)

$(OUTPUTS): %: %.ml
	$(OCAMLC) -o $@ $@.ml
	rm -f *.cmx *.cmi *.o

clean:
	rm -f *.cmx *.cmi *.o $(OUTPUTS)
