CRYSTAL=crystal
CRYSTAL_FLAGS=--release
EXE=nqueen matmul bedcov

all:$(EXE)

nqueen:nqueen.cr
	$(CRYSTAL) build $(CRYSTAL_FLAGS) $<

matmul:matmul.cr
	$(CRYSTAL) build $(CRYSTAL_FLAGS) $<

bedcov:bedcov.cr
	$(CRYSTAL) build $(CRYSTAL_FLAGS) $<

clean:
	rm -f $(EXE) *.dwarf
