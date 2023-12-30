VFLAGS=-prod
VC=v
EXE=matmul

all:$(EXE)

matmul:matmul.v
	$(VC) $(VFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
