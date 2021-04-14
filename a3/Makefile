OCAMLC=ocamlopt
OUTPUTS=find_duplicate rotten_tomatoes

all: $(OUTPUTS)

$(OUTPUTS): %: %.ml
	$(OCAMLC) -o $@ $@.ml
	rm -f *.cmx *.cmi *.o

clean:
	rm -f *.cmx *.cmi *.o $(OUTPUTS)
