all: memory3 memory4 cars replay

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -Icoin-Osi/include/coin -I/usr/local/mercury-11.07/lib/mercury/inc/ -c coin-clp.cc

cars: coin-clp.o *.m
	mmc --make --grade hlc.par.gc --parallel --link-object coin-clp.o -R coin-Osi/lib -L coin-Osi/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++ cars

memory3: coin-clp.o memory3.m
	mmc --make --grade asm_fast.par.gc --link-object coin-clp.o -R coin-Osi/lib -L coin-Osi/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++ memory3

memory4: coin-clp.h coin-clp.cc memory4.cc
	## Mercury Boehm, mit GC in COIN:
	#g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mercury-gc/include/coin -I/home/chs/Programs/mercury-compiler-11.07.1/dist/lib/mercury/inc/ -Lcoin-Osi-laeuft-mit-memory4-mercury-gc/lib -L/home/chs/Programs/mercury-compiler-11.07.1/dist/lib/mercury/lib/ -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lpar_gc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mercury-gc/lib:/home/chs/Programs/mercury-compiler-11.07.1/dist/lib/mercury/lib/
	#
	# Mercury Boehm, kein GC in COIN:
	#g++ -Wall -Icoin-Osi/include/coin -I/home/chs/Programs/mercury-compiler-11.07.1/dist/lib/mercury/inc/ -Lcoin-Osi/lib -L/home/chs/Programs/mercury-compiler-11.07.1/dist/lib/mercury/lib/ -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lpar_gc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi/lib:/home/chs/Programs/mercury-compiler-11.07.1/dist/lib/mercury/lib/
	#
	# Mein Boehm, mit GC in COIN:
	g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mein-gc/include/coin -Igc-7.1/dist/include -Lcoin-Osi-laeuft-mit-memory4-mein-gc/lib -Lgc-7.1/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mein-gc/lib:gc-7.1/dist/lib
	#
	# Mein Boehm, kein GC in COIN:
	#g++ -Wall -Icoin-Osi/include/coin -Igc-7.1/dist/include -Lcoin-Osi/lib -Lgc-7.1/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi/lib:gc-7.1/dist/lib


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

