ZIG=zig
ZIG_FLAGS=-O ReleaseFast
EXE=nqueen matmul

all:$(EXE)

nqueen:nqueen.zig
	$(ZIG) build-exe $(ZIG_FLAGS) $<

matmul:matmul.zig
	$(ZIG) build-exe $(ZIG_FLAGS) $<

clean:
	rm -f $(EXE) *.o
