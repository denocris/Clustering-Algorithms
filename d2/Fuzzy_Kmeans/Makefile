FC = gfortran
FFLAGS = -Wall

.SUFFIXES:



all: fuzkmeanspp

fuzkmeanspp: fuzkmeanspp.x
	./fuzkmeanspp.x < data/s3.dat

fuzkmeanspp.x: tools.o fuzkmeanspp.o
	$(FC) $^ -o $@


%.o: %.f03
	$(FC) -c $< $(FFLAGS) -o $@  # metti @ all'inizio per togliere warning

#%.o: %.f90
#	$(FC) -c $< $(FFLAGS) -o $@


clean:
	rm -f *.o *~ *.x *.out *.bak *.txt *.mod

.PHONY: clean
