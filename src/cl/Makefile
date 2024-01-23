LISP ?= sbcl
EXE=nqueen matmul sudoku

all:$(EXE)

nqueen:nqueen.asd
	$(LISP) --eval "(require :asdf)" \
		--load $< \
		--eval "(asdf:make :$@)" \
		--eval "(quit)"

matmul:matmul.asd
	$(LISP) --eval "(require :asdf)" \
		--load $< \
		--eval "(asdf:make :$@)" \
		--eval "(quit)"

sudoku:sudoku.asd
	$(LISP) --eval "(require :asdf)" \
		--load $< \
		--eval "(asdf:make :$@)" \
		--eval "(quit)"

clean:
	rm -f $(EXE)
