FC=gfortran
FFLAGS=-O3
EXE=nqueen matmul sudoku

all:$(EXE)

matmul:matmul.F90
	$(FC) $(FFLAGS) -o $@ $<

nqueen:nqueen.F90
	$(FC) $(FFLAGS) -o $@ $<

sudoku:sudoku.F90
	$(FC) $(FFLAGS) -o $@ $<

clean:
	rm -f $(EXE) *.mod
