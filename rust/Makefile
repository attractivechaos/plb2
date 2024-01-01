RUSTC=rustc
EXE=nqueen sudoku

all:$(EXE)

sudoku:sudoku.rs
	$(RUSTC) -O -o $@ $<

nqueen:nqueen.rs
	$(RUSTC) -O -o $@ $<

clean:
	rm -f $(EXE)
