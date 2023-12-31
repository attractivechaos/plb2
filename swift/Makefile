SWIFTC=swiftc
EXE=nqueen sudoku

all:$(EXE)

nqueen:nqueen.swift
	$(SWIFTC) -O $<

sudoku:sudoku.swift
	$(SWIFTC) -O $<

clean:
	rm -f $(EXE)
