MOJO=mojo
EXE=nqueen matmul

all:$(EXE)

nqueen:nqueen.mojo
	$(MOJO) build -o $@ $<

matmul:matmul.mojo
	$(MOJO) build -o $@ $<

clean:
	rm -f $(EXE)
