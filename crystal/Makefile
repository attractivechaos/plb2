CRYSTAL=crystal
EXE=nqueen matmul

all:$(EXE)

matmul:matmul.cr
	$(CRYSTAL) build --release $<

nqueen:nqueen.cr
	$(CRYSTAL) build --release $<

clean:
	rm -f $(EXE) *.dwarf
