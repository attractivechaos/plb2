EXE=matmul
OCAML=ocamlopt

all:$(EXE)

matmul:matmul.ml
	$(OCAML) -o $@ $<

clean:
	rm -f $(EXE) *.cmi *.cmx *.o
