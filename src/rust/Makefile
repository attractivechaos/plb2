RUSTC=rustc
RUST_FLAGS=-C debuginfo=0 -C opt-level=3
EXE=nqueen sudoku matmul bedcov

all:$(EXE)

sudoku:sudoku.rs
	$(RUSTC) $(RUST_FLAGS) -o $@ $<

nqueen:nqueen.rs
	$(RUSTC) $(RUST_FLAGS) -o $@ $<

matmul:matmul.rs
	$(RUSTC) $(RUST_FLAGS) -o $@ $<

bedcov:bedcov.rs
	$(RUSTC) $(RUST_FLAGS) -o $@ $<

clean:
	rm -f $(EXE)
