all: invert gauss-jordan determinant

objs = gauss.o mats.o
mods = gauss.mod mats.mod

determinant: determinant.f90 $(objs)
	gfortran -o determinant determinant.f90 $(objs)

gauss-jordan: gauss-jordan.f90 $(objs)
	gfortran -o gauss-jordan gauss-jordan.f90 $(objs)

invert: invert.f90 $(objs)
	gfortran -o invert invert.f90 $(objs)

gauss.o: gauss.f90
	gfortran -c gauss.f90

mats.o: mats.f90
	gfortran -c mats.f90

clean:
	rm determinant gauss-jordan invert $(objs) $(mods)
