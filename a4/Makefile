OCAMLC=ocamlopt
OUTPUTS=weather_generator

all: $(OUTPUTS)

$(OUTPUTS): %: %.ml
	$(OCAMLC) -o $@ str.cmxa $@.ml
	rm -f *.cmx *.cmi *.o

clean:
	rm -f *.cmx *.cmi *.o $(OUTPUTS)
