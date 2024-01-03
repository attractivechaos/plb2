LISP ?= sbcl
EXE=nqueen

all:$(EXE)

nqueen:nqueen.asd
	$(LISP) --load $< \
		--eval "(ql:quickload :$@)" \
		--eval "(asdf:make :$@)" \
		--eval "(quit)"

clean:
	rm -f $(EXE)
