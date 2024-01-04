DOTNET_ROOT=/usr/local/share/dotnet
CSFLAGS=-c Release -r osx-arm64
EXE=nqueen matmul sudoku

all:$(EXE)

nqueen:nqueen.cs
	$(DOTNET_ROOT)/dotnet publish $(CSFLAGS) -o bin $<proj && mv bin/$@ .

matmul:matmul.cs
	$(DOTNET_ROOT)/dotnet publish $(CSFLAGS) -o bin $<proj && mv bin/$@ .

sudoku:sudoku.cs
	$(DOTNET_ROOT)/dotnet publish $(CSFLAGS) -o bin $<proj && mv bin/$@ .

clean:
	rm -fr bin obj $(EXE) *.dll *.json
