FC=f95
FFLAGS=-O3 -g
EXE=sudoku nqueen matmul bedcov

all:$(EXE)

matmul:matmul.F90
	$(FC) $(FFLAGS) -o $@ $<

sudoku:sudoku.F90
	$(FC) $(FFLAGS) -o $@ $<

nqueen:nqueen.F90
	$(FC) $(FFLAGS) -o $@ $<

bedcov:bedcov.F90
	$(FC) $(FFLAGS) -o $@ $<

clean:
	rm -f $(EXE)
	rm -f *.mod
