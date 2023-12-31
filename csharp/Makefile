DOTNET_ROOT=/usr/local/share/dotnet
EXE=nqueen

all:$(EXE)

nqueen:nqueen.cs
	$(DOTNET_ROOT)/dotnet build -c Release nqueen.csproj && cp bin/Release/net8.0/nqueen{,.dll,.runtimeconfig.json} .

clean:
	rm -fr bin obj $(EXE) *.dll *.json
