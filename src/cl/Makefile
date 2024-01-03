LISP ?= sbcl
EXE=nqueen matmul

all:$(EXE)

nqueen:nqueen.asd
	$(LISP) --load $< \
		--eval "(ql:quickload :$@)" \
		--eval "(asdf:make :$@)" \
		--eval "(quit)"

matmul:matmul.asd
	$(LISP) --load $< \
		--eval "(ql:quickload :$@)" \
		--eval "(asdf:make :$@)" \
		--eval "(quit)"

clean:
	rm -f $(EXE)
