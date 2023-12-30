EXE=nqueen-dlx nqueen

all:$(EXE)

nqueen:nqueen.go
	go build $<

nqueen-dlx:nqueen-dlx.go
	echo 'GOPATH=`pwd` go build $<' | sh

clean:
	rm -f $(EXE)
