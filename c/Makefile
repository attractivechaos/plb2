CFLAGS=-O3
EXE=sudoku nqueen

all:$(EXE)

sudoku:sudoku.c
	$(CC) $(CFLAGS) -o $@ $<

nqueen:nqueen.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
