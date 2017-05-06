FC = gfortran
FFLAGS = -Wall

.SUFFIXES:



all: kmeanspp


kmeans: kmeans.x
	./kmeans.x < data/s3.dat

kmeanspp: kmeanspp.x
	./kmeanspp.x < data/s3.dat

kmeans.x: tools.o kmeans.o
	$(FC) $^ -o $@

kmeanspp.x: tools.o kmeanspp.o
	$(FC) $^ -o $@


%.o: %.f03
	$(FC) -c $< $(FFLAGS) -o $@  # metti @ all'inizio per togliere warning

#%.o: %.f90
#	$(FC) -c $< $(FFLAGS) -o $@


clean:
	rm -f *.o *~ *.x *.out *.bak *.txt

.PHONY: clean
