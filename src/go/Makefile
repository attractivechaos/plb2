EXE=nqueen sudoku matmul bedcov

all:$(EXE)

nqueen:nqueen.go
	go build $<

matmul:matmul.go
	go build $<

sudoku:sudoku.go
	go build $<

bedcov:bedcov.go
	go build $<

clean:
	rm -f $(EXE)
