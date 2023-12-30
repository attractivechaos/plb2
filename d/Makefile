LDC=ldc2
EXE=nqueen

all:$(EXE)

nqueen:nqueen.d
	$(LDC) -O3 -release $<

clean:
	rm -f $(EXE) *.o
