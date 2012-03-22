all: cars replay

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -Icoin-Osi/include/coin -c coin-clp.cc

cars: coin-clp.o *.m
	mmc --make --link-object coin-clp.o -R coin-Osi/lib -L coin-Osi/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -lstdc++ cars

maze: *.m
	mmc --make --link-object coin-clp.o -R coin-Osi/lib -L coin-Osi/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -lstdc++ maze

replay: replay.c
	cc -Wall -O3 -o replay replay.c

clean:
	rm -rf Mercury
	rm -f *.mh
	rm -f *.err
	rm -f cars maze
	rm -f Prof.*

