SWIFTC=swiftc
SWIFTC_FLAGS=-O
EXE=nqueen matmul sudoku

all:$(EXE)

nqueen:nqueen.swift
	$(SWIFTC) $(SWIFTC_FLAGS) $<

matmul:matmul.swift
	$(SWIFTC) $(SWIFTC_FLAGS) $<

sudoku:sudoku.swift
	$(SWIFTC) $(SWIFTC_FLAGS) $<

clean:
	rm -f $(EXE)
