GOAL = prgolog

LIB_MCFLAGS = --ml osi

all: lib

lib: osi
lib: *.m

osi:
	make -C ../osi

include ../Make.common

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -fPIC -I${MERCURY}/lib/mercury/inc -I${COIN}/include/coin -c coin-clp.cc

