GOAL = osi
SOURCE_FILES = $(shell ls *.m *.cc *.h)

# For non-standalone OSI solver, activate the following lines:
# --no-as-needed is for some reason needed on Rambo (gcc 4.6.1)
# COIN = $(shell pwd)/coin-Osi-gc
# LIB_MCFLAGS = --extra-library-header coin-clp.h --ld-libflags -Wl,--no-as-needed --link-object coin-clp.o -R${COIN}/dist/lib -L${COIN}/dist/lib -lClp -lOsi -lOsiClp -lCoinUtils -lblas -lm -lstdc++
# lib: coin-Osi coin-clp.o

all: mercury-depends lib

lib: *.m

include ../Make.common

coin-Osi:
	./install_coin_osi.sh "$(COIN)" "$(MERCURY_SRC)" "$(MERCURY_INST)"

coin-clp.o: coin-clp.cc coin-clp.h
	g++ -Wall -fPIC -I${MERCURY_SRC}/dist/lib/mercury/inc -I${COIN}/dist/include/coin -c coin-clp.cc

