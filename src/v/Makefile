VFLAGS=-prod
VC=v
EXE=matmul nqueen sudoku bedcov

all:$(EXE)

matmul:matmul.v
	$(VC) $(VFLAGS) -o $@ $<

nqueen:nqueen.v
	$(VC) $(VFLAGS) -o $@ $<

sudoku:sudoku.v
	$(VC) $(VFLAGS) -o $@ $<

bedcov:bedcov.v
	$(VC) $(VFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
