CRYSTAL=crystal
EXE=nqueen

all:$(EXE)

nqueen:nqueen.cr
	$(CRYSTAL) build --release $<

clean:
	rm -f $(EXE) *.dwarf
