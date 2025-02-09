CHPL=chpl
CHPLFLAGS=--fast
EXE=nqueen matmul

all:$(EXE)

matmul:matmul.chpl
	$(CHPL) $(CHPLFLAGS) -o $@ $<

nqueen:nqueen.chpl
	$(CHPL) $(CHPLFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
