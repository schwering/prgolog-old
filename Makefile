MERCURY=/home/chs/Programs/mercury-compiler-11.07.1/dist
COIN=coin-Osi-laeuft-mit-memory4-mercury-par_gc-durch-configure

all: memory3 memory4 cars replay

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -I${MERCURY}/lib/mercury/inc -I${COIN}/include/coin -c coin-clp.cc

cars: coin-clp.o *.m
	mmc --make --grade hlc.par.gc --parallel --link-object coin-clp.o -R ${COIN}/lib -L ${COIN}/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++ cars

memory3: coin-clp.o memory3.m
	mmc --make --grade hlc.par.gc --link-object coin-clp.o -R ${COIN}/lib -L ${COIN}/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++ memory3

memory4: coin-clp.h coin-clp.cc memory4.cc
	# Mercury Boehm, mit GC in COIN:
	#g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mercury-gc/include/coin -I${MERCURY}/lib/mercury/inc/ -Lcoin-Osi-laeuft-mit-memory4-mercury-gc/lib -L${MERCURY}/lib/mercury/lib/ -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lpar_gc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mercury-gc/lib:${MERCURY}/lib/mercury/lib/
	#
	# Mercury Boehm, kein GC in COIN:
	#g++ -Wall -Icoin-Osi/include/coin -I${MERCURY}/lib/mercury/inc/ -Lcoin-Osi/lib -L${MERCURY}/lib/mercury/lib/ -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lpar_gc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi/lib:${MERCURY}/lib/mercury/lib/
	#
	# Mein Boehm 7.1, mit GC in COIN:
	#g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mein-gc-7.1/include/coin -Igc-7.1/dist/include -Lcoin-Osi-laeuft-mit-memory4-mein-gc-7.1/lib -Lgc-7.1/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mein-gc-7.1/lib:gc-7.1/dist/lib
	#
	# Mein Boehm 7.1, kein GC in COIN:
	#g++ -Wall -Icoin-Osi/include/coin -Igc-7.1/dist/include -Lcoin-Osi/lib -Lgc-7.1/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi/lib:gc-7.1/dist/lib
	#
	# Mein Boehm 7.2alpha4, mit GC in COIN:
	#g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mein-gc-7.2alpha4/include/coin -Igc-7.2alpha4/dist/include -Lcoin-Osi-laeuft-mit-memory4-mein-gc-7.2alpha4/lib -Lgc-7.2alpha4/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mein-gc-7.2alpha4/lib:gc-7.2alpha4/dist/lib
	#
	# Mein Boehm 7.2alpha4, kein GC in COIN:
	#g++ -Wall -Icoin-Osi/include/coin -Igc-7.2alpha4/dist/include -Lcoin-Osi/lib -Lgc-7.2alpha4/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi/lib:gc-7.2alpha4/dist/lib
	#
	# Mein Boehm merc, mit GC in COIN:
	#g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mein-gc-merc/include/coin -Igc-merc/dist/include -Lcoin-Osi-laeuft-mit-memory4-mein-gc-merc/lib -Lgc-merc/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mein-gc-merc/lib:gc-merc/dist/lib
	#
	# Mein Boehm merc, kein GC in COIN:
	#g++ -Wall -Icoin-Osi/include/coin -Igc-merc/dist/include -Lcoin-Osi/lib -Lgc-merc/dist/lib -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi/lib:gc-merc/dist/lib
	#
	#
	# Mein Boehm merc mmake, kein GC in COIN:
	#g++ -Wall -Icoin-Osi/include/coin -Igc-merc-mmake/include -Lcoin-Osi/lib -Lgc-merc-mmake -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mein-gc-merc/lib:gc-merc-mmake
	#
	#
	# Mercurys Boehm aus boehm_gc/
	#g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mercury-gc/include/coin -I${MERCURY}/../boehm_gc/include -Lcoin-Osi-laeuft-mit-memory4-mercurys-boehm-gc/lib -L${MERCURY}/../boehm_gc -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lgc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mercurys-boehm-gc/lib:${MERCURY}/../boehm_gc
	#
	#
	#
	# Mercury Boehm durch boehm_gc + configure ersetzt, mit GC in COIN:
	g++ -Wall -Icoin-Osi-laeuft-mit-memory4-mercury-par_gc-durch-configure/include/coin -I${MERCURY}/lib/mercury/inc/ -Lcoin-Osi-laeuft-mit-memory4-mercury-par_gc-durch-configure/lib -L${MERCURY}/lib/mercury/lib/ -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lpar_gc memory4.cc coin-clp.cc -o memory4
	export LD_LIBRARY_PATH=coin-Osi-laeuft-mit-memory4-mercury-par_gc-durch-configure/lib:${MERCURY}/lib/mercury/lib/
	#
	#
	#g++ -Wall -I${COIN}/include/coin -I${MERCURY}/lib/mercury/inc/ -L${COIN}/lib -L${MERCURY}/lib/mercury/lib/ -lClp -lOsiClp -lOsi -lCoinUtils -lblas -lm -lpthread -ldl -lpar_gc memory4.cc coin-clp.cc -o memory4
	#export LD_LIBRARY_PATH=${COIN}/lib:${MERCURY}/lib/mercury/lib/


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

