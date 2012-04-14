MERCURY = $(shell dirname $(shell which mmc))/../..
COIN = $(shell pwd)/coin-Osi-gc
GOAL = osi
SOURCE_FILES = $(shell ls *.m *.cc *.h)

# --no-as-needed is for some reason needed on Rambo (gcc 4.6.1)
LIB_MCFLAGS = --extra-library-header coin-clp.h --ld-libflags -Wl,--no-as-needed --link-object coin-clp.o -R ${COIN}/dist/lib -L ${COIN}/dist/lib -lClp -lOsi -lOsiClp -lCoinUtils -lblas -lm -lstdc++

all: lib

lib: coin_or *.m coin-clp.o

coin_or:
	./install_coin_osi.sh "$(COIN)" "$(MERCURY)" "$(MERCURY)/dist"

include ../Make.common

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -fPIC -I${MERCURY}/dist/lib/mercury/inc -I${COIN}/dist/include/coin -c coin-clp.cc

