FC = gfortran
FFLAGS = -Wall

.SUFFIXES:



all: kmeans


kmeans: kmeans.x
	./kmeans.x < data/Aggregation.txt
	#./kmeans.x < data/s3.dat

kmeans.x: tools.o kmeans.o
	$(FC) $^ -o $@



%.o: %.f03
	$(FC) -c $< $(FFLAGS) -o $@  # metti @ all'inizio per togliere warning



clean:
	rm -f *.o *~ *.x *.out *.bak *.txt

.PHONY: clean
