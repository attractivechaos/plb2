DOTNET_ROOT=/usr/local/share/dotnet
CSFLAGS=-c Release -r osx-arm64
EXE=nqueen matmul sudoku

all:$(EXE)

nqueen:nqueen.cs
	$(DOTNET_ROOT)/dotnet build $(CSFLAGS) nqueen.csproj && cp bin/Release/*/*/$@{,.runtimeconfig.json,.dll} .

matmul:matmul.cs
	$(DOTNET_ROOT)/dotnet build $(CSFLAGS) matmul.csproj && cp bin/Release/*/*/$@{,.runtimeconfig.json,.dll} .

sudoku:sudoku.cs
	$(DOTNET_ROOT)/dotnet build $(CSFLAGS) sudoku.csproj && cp bin/Release/*/*/$@{,.runtimeconfig.json,.dll} .

clean:
	rm -fr bin obj $(EXE) *.dll *.json
