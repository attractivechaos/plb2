CFLAGS=-O3
EXE=sudoku nqueen matmul

all:$(EXE)

matmul:matmul.c
	$(CC) $(CFLAGS) -o $@ $<

sudoku:sudoku.c
	$(CC) $(CFLAGS) -o $@ $<

nqueen:nqueen.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
