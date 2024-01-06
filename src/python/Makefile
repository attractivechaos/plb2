CODON=codon
CODON_FLAGS=-release -exe

EXE=nqueen matmul sudoku

all:$(EXE)

matmul:matmul.py
	$(CODON) build $(CODON_FLAGS) -o $@ $<

nqueen:nqueen.py
	$(CODON) build $(CODON_FLAGS) -o $@ $<

sudoku:sudoku.py
	$(CODON) build $(CODON_FLAGS) -o $@ $<

bedcov:bedcov.py
	$(CODON) build $(CODON_FLAGS) -o $@ $<

clean:
	rm -f $(EXE)
