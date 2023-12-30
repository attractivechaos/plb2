RUSTC=rustc
EXE=nqueen

all:$(EXE)

nqueen:nqueen.rs
	$(RUSTC) -O -o $@ $<

clean:
	rm -f $(EXE)
