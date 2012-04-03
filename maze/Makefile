all: maze

maze: *.m
	mmc --make --mld ../lib/mercury --ml prgolog --grade hlc.par.gc --parallel maze

clean:
	rm -f cars
	rm -rf Mercury
	rm -f *.o
	rm -f *.mh
	rm -f *.err
	rm -f cars maze
	rm -f Prof.*

