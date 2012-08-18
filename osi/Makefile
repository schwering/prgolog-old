COIN = $(shell pwd)/coin-Osi-gc
GOAL = osi
SOURCE_FILES = $(shell ls *.m)

# For non-standalone OSI solver, activate the following lines:
#SOURCE_FILES = $(shell ls *.m *.cc *.h)
# --no-as-needed is for some reason needed on Rambo (gcc 4.6.1)
#LIB_MCFLAGS = --extra-library-header coin-clp.h --ld-libflags -Wl,--no-as-needed --link-object coin-clp.o -R${COIN}/dist/lib -L${COIN}/dist/lib -lClp -lOsi -lOsiClp -lCoinUtils -lblas -lm -lstdc++
#lib: mercury-depends coin_or *.m coin-clp.o

all: lib

lib: mercury-depends *.m

coin_or:
	./install_coin_osi.sh "$(COIN)" "$(MERCURY_SRC)" "$(MERCURY_INST)"

include ../Make.common

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -fPIC -I${MERCURY_SRC}/dist/lib/mercury/inc -I${COIN}/dist/include/coin -c coin-clp.cc

