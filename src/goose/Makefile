GOOSE=goose
GOOSE_FLAGS=-O2
CC=clang
CFLAGS=-O3
EXE=nqueen matmul sudoku bedcov

all:$(EXE)

# Goose compiles each program to one C file, which a C compiler builds.
%:%.goose
	$(GOOSE) $(GOOSE_FLAGS) --standalone -o $@.c $<
	$(CC) $(CFLAGS) -w -o $@ $@.c -lm -pthread

clean:
	rm -f $(EXE) $(EXE:=.c)
