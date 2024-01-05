EXE=matmul nqueen
OCAML=ocamlopt

all:$(EXE)

matmul:matmul.ml
	$(OCAML) -o $@ $<

nqueen:nqueen.ml
	$(OCAML) -o $@ $<

clean:
	rm -f $(EXE) *.cmi *.cmx *.o
