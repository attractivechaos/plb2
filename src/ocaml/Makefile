EXE=matmul nqueen
OCAML=ocamlopt
OCAML_FLAGS=-unsafe

all:$(EXE)

matmul:matmul.ml
	$(OCAML) $(OCAML_FLAGS) -o $@ $<

nqueen:nqueen.ml
	$(OCAML) $(OCAML_FLAGS) -o $@ $<

clean:
	rm -f $(EXE) *.cmi *.cmx *.o
