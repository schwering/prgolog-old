MERCURY = /home/chs/Programs/mercury-compiler-11.07.1/dist
COIN = /home/chs/Documents/Prolog/mercury/osi/coin-Osi-laeuft-mit-memory4-mercury-par_gc-durch-configure
GOAL = osi

LIB_MCFLAGS = --extra-library-header coin-clp.h --link-object coin-clp.o -R ${COIN}/lib -L ${COIN}/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++

all: lib

lib: coin-clp.o
lib: *.m

include ../Make.common

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -fPIC -I${MERCURY}/lib/mercury/inc -I${COIN}/include/coin -c coin-clp.cc

