OCAMLC=ocamlopt
OUTPUTS=scenario_analysis

all: $(OUTPUTS)

$(OUTPUTS): %: %.ml
	$(OCAMLC) -o $@ str.cmxa fuel.ml lease.ml vehicle.ml scenario_analysis.ml

clean:
	rm -f *.cmx *.cmi *.o $(OUTPUTS)
