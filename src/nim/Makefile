NIM=nim
NIM_FLAGS=-d:release -d:lto --bound_checks:off --overflow_checks:off -d:nimEmulateOverflowChecks
EXE=nqueen matmul bedcov sudoku

all:$(EXE)

nqueen:nqueen.nim
	$(NIM) c $(NIM_FLAGS) -o:$@ $<

matmul:matmul.nim
	$(NIM) c $(NIM_FLAGS) -o:$@ $<

bedcov:bedcov.nim
	$(NIM) c $(NIM_FLAGS) -o:$@ $<

sudoku:sudoku.nim
	$(NIM) c $(NIM_FLAGS) -o:$@ $<

clean:
	rm -f $(EXE)
