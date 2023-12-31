MOJO=mojo
EXE=nqueen

all:$(EXE)

nqueen:nqueen.mojo
	$(MOJO) build -o $@ $<

clean:
	rm -f $(EXE)
