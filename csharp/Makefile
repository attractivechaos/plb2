DOTNET_ROOT=/usr/local/share/dotnet
EXE=nqueen sudoku

all:$(EXE)

nqueen:nqueen.cs
	$(DOTNET_ROOT)/dotnet build -c Release nqueen.csproj && cp bin/Release/net8.0/nqueen{,.dll,.runtimeconfig.json} .

sudoku:sudoku.cs
	$(DOTNET_ROOT)/dotnet build -c Release sudoku.csproj && cp bin/Release/net8.0/sudoku{,.dll,.runtimeconfig.json} .

clean:
	rm -fr bin obj $(EXE) *.dll *.json
