all: invert gauss-jordan

objs = gauss.o mats.o
mods = gauss.mod mats.mod

gauss-jordan: gauss-jordan.f90 $(objs)
	gfortran -o gauss-jordan gauss-jordan.f90 $(objs)

invert: invert.f90 $(objs)
	gfortran -o invert invert.f90 $(objs)

gauss.o: gauss.f90
	gfortran -c gauss.f90

mats.o: mats.f90
	gfortran -c mats.f90

clean:
	rm gauss-jordan invert $(objs) $(mods)
