LDC=ldc2
DFLAGS=-O3 -release -flto=full --boundscheck=off
EXE=nqueen matmul sudoku bedcov

all:$(EXE)

nqueen:nqueen.d
	$(LDC) $(DFLAGS) $<

matmul:matmul.d
	$(LDC) $(DFLAGS) $<

sudoku:sudoku.d
	$(LDC) $(DFLAGS) $<

bedcov:bedcov.d
	$(LDC) $(DFLAGS) $<

clean:
	rm -f $(EXE) *.o
