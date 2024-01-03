DART=dart
EXE=nqueen matmul sudoku

all:$(EXE)

nqueen:nqueen.dart
	$(DART) compile exe -o $@ $<

matmul:matmul.dart
	$(DART) compile exe -o $@ $<

sudoku:sudoku.dart
	$(DART) compile exe -o $@ $<

clean:
	rm -f $(EXE) *.o
