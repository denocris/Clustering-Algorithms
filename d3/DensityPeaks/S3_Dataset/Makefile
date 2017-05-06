FC = gfortran
FFLAGS = -Wall

.SUFFIXES:

all: dpeaks

dpeaks: dpeaks.x
	./dpeaks.x < data/s3.dat
	#./dpeaks.x < data/s3.dat

dpeaks.x: tools.o dpeaks.o
	$(FC) $^ -o $@


%.o: %.f03
	$(FC) -c $< $(FFLAGS) -o $@  # metti @ all'inizio per togliere warning

#%.o: %.f90
#	$(FC) -c $< $(FFLAGS) -o $@


clean:
	rm -f *.o *~ *.x *.out *.bak

.PHONY: clean
