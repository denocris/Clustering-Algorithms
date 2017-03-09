FC = gfortran
FFLAGS = -Wall

.SUFFIXES:

all: 01

01: kmeans.x
	./kmeans.x < s3.dat

kmeans.x: tools.o kmeans.o
	$(FC) $^ -o $@


%.o: %.f03
	$(FC) -c $< $(FFLAGS) -o $@  # metti @ all'inizio per togliere warning

#%.o: %.f90
#	$(FC) -c $< $(FFLAGS) -o $@


clean:
	rm -f *.o *~ *.x *.out *.txt

.PHONY: clean
