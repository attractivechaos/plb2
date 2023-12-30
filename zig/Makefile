ZIG=zig
EXE=nqueen

all:$(EXE)

nqueen:nqueen.zig
	$(ZIG) build-exe -O ReleaseFast $<

clean:
	rm -f $(EXE) *.o
