NIM=nim
NIM_FLAGS=--d:danger --passL: "-flto -march=native" --passC: "-flto"
EXE=nqueen matmul bedcov

all:$(EXE)

nqueen:nqueen.nim
	$(NIM) c $(NIM_FLAGS) -o:$@ $<

matmul:matmul.nim
	$(NIM) c $(NIM_FLAGS) -o:$@ $<

bedcov:bedcov.nim
	$(NIM) c $(NIM_FLAGS) -o:$@ $<

clean:
	rm -f $(EXE)
