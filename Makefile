all: invert reduce

objs = gauss.o mats.o
mods = gauss.mod mats.mod

reduce: reduce.f90 $(objs)
	gfortran -o reduce reduce.f90 $(objs)

invert: invert.f90 $(objs)
	gfortran -o invert invert.f90 $(objs)

gauss.o: gauss.f90
	gfortran -c gauss.f90

mats.o: mats.f90
	gfortran -c mats.f90

clean:
	rm reduce invert $(objs) $(mods)
