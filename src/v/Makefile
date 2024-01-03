VFLAGS=-prod -gc none
VC=v
EXE=matmul nqueen

all:$(EXE)

matmul:matmul.v
	$(VC) $(VFLAGS) -o $@ $<

nqueen:nqueen.v
	$(VC) $(VFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
