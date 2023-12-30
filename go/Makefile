EXE=nqueen-dlx

all:$(EXE)

nqueen-dlx:nqueen-dlx.go
	echo 'GOPATH=`pwd` go build $<' | sh

clean:
	rm -f $(EXE)
