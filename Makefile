all: memory3 memory4 cars replay

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -Icoin-Osi/include/coin -Igc-7.1/dist/include -c coin-clp.cc

cars: coin-clp.o *.m
	mmc --make --grade hlc.par.gc --parallel --link-object coin-clp.o -R coin-Osi/lib -L coin-Osi/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++ cars

memory3: coin-clp.o memory3.m
	mmc --make --grade hlc.par.gc --parallel --link-object coin-clp.o -R coin-Osi/lib -L coin-Osi/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++ memory3

memory4: coin-clp.o memory4.cc
	#g++ -Wall -Icoin-Osi/include/coin -I/usr/local/mercury-11.07/lib/mercury/inc/ -Lcoin-Osi/lib -L/usr/local/mercury-11.07/lib/mercury/lib/ -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	g++ -Wall -Icoin-Osi/include/coin -Igc-7.1/dist/include -Lcoin-Osi/lib -Lgc-7.1/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.o -o memory4

maze: *.m
	mmc --make --link-object coin-clp.o -R coin-Osi/lib -L coin-Osi/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -lstdc++ maze

replay: replay.c
	cc -Wall -O3 -o replay replay.c

clean:
	rm -rf Mercury
	rm -f *.o
	rm -f *.mh
	rm -f *.err
	rm -f cars maze
	rm -f Prof.*

