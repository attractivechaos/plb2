CFLAGS=-O3 -fwrapv -flto
EXE=sudoku nqueen matmul bedcov

all:$(EXE)

matmul:matmul.c
	$(CC) $(CFLAGS) -o $@ $<

sudoku:sudoku.c
	$(CC) $(CFLAGS) -o $@ $<

nqueen:nqueen.c
	$(CC) $(CFLAGS) -o $@ $<

bedcov:bedcov.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
