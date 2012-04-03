GOAL = prgolog
SOURCE_FILES = $(shell ls *.m)

LIB_MCFLAGS = --ml osi

all: lib

lib: *.m osi

osi:
	make -C ../osi

include ../Make.common

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -fPIC -I${MERCURY}/lib/mercury/inc -I${COIN}/include/coin -c coin-clp.cc

