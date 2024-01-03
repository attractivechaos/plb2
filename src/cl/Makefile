LISP ?= sbcl
EXE=nqueen matmul

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

clean:
	rm -f $(EXE)
