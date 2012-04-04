MERCURY = /home/chs/Programs/mercury-compiler-11.07.1
COIN = /home/chs/Documents/Prolog/mercury/osi/coin-Osi-gc
GOAL = osi
SOURCE_FILES = $(shell ls *.m *.cc *.h)

LIB_MCFLAGS = --extra-library-header coin-clp.h --link-object coin-clp.o -R ${COIN}/dist/lib -L ${COIN}/dist/lib -l Clp -l OsiClp -l Osi -l CoinUtils -l blas -l m -l stdc++

all: lib

lib: coin_or *.m coin-clp.o

coin_or:
	./install_coin_osi.sh "$(COIN)" "$(MERCURY)" "$(MERCURY)/dist"

include ../Make.common

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -fPIC -I${MERCURY}/dist/lib/mercury/inc -I${COIN}/dist/include/coin -c coin-clp.cc

