NIM=nim
EXE=nqueen

all:$(EXE)

nqueen:nqueen.nim
	$(NIM) c -d:release -d:nimEmulateOverflowChecks --bound_checks:off -o:$@ $<

clean:
	rm -f $(EXE)
