LDC=ldc2
EXE=nqueen sudoku

all:$(EXE)

nqueen:nqueen.d
	$(LDC) -O3 -release $<

sudoku:sudoku.d
	$(LDC) -O3 -release $<

clean:
	rm -f $(EXE) *.o
