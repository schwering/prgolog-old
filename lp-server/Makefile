COIN = $(shell pwd)/coin-Osi
GOAL = lps
HEADER_FILES = $(shell ls *.h)
BODY_FILES = $(shell ls *.cc)
SOURCE_FILES = $(HEADER_FILES) $(BODY_FILES)
OBJECT_FILES = $(BODY_FILES:%.cc=%.o)

CXXFLAGS = -Wall
CPPFLAGS = -I$(COIN)/dist/include/coin
LD	 = g++
LDFLAGS	 = -Wl,--no-as-needed -Wl,-rpath=$(COIN)/dist/lib -L$(COIN)/dist/lib -lClp -lOsi -lOsiClp -lCoinUtils -lblas -lm -lpthread

DEPEND          = makedepend
CDEPEND         = $(DEPEND) -Y. -- $(CPPFLAGS) --
CXXDEPEND       = $(DEPEND) -Y. -- $(CPPFLAGS) --

all: $(COIN)/dist exe

exe: $(OBJECT_FILES)
	$(LD) $(LDFLAGS) -o lps $(OBJECT_FILES)

$(COIN)/dist:
	cd "$(COIN)"
	./configure "--prefix=$(COIN)/dist"
	make
	make install
	cd ..

depend: $(SOURCE_FILES)
	$(CXXDEPEND) $(SOURCE_FILES)

clean:
	rm -f *.o

# DO NOT DELETE

coin-clp.o: lp-types.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiClpSolverInterface.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpSimplex.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpModel.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpConfig.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/config_clp.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpPackedMatrix.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPragma.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpMatrixBase.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinFinite.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinUtilsConfig.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/config_coinutils.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPackedMatrix.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinError.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPackedVectorBase.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinFloatEqual.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinShallowPackedVector.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinMessageHandler.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinHelperFunctions.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpParameters.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpObjective.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpSolve.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpLinearObjective.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiSolverInterface.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiCollections.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiSolverParameters.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinWarmStartBasis.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinSort.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinDistance.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinWarmStart.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpEventHandler.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpNode.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinIndexedVector.hpp
lp-msg.o: lp-types.h
coin-clp.o: coin-clp.h lp-types.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiClpSolverInterface.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpSimplex.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpModel.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpConfig.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/config_clp.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpPackedMatrix.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPragma.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpMatrixBase.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinFinite.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinUtilsConfig.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/config_coinutils.h
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPackedMatrix.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinError.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPackedVectorBase.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinFloatEqual.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinShallowPackedVector.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinMessageHandler.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinHelperFunctions.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpParameters.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpObjective.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpSolve.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpLinearObjective.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiSolverInterface.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiCollections.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiSolverParameters.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinWarmStartBasis.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinSort.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinDistance.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinWarmStart.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpEventHandler.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpNode.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinIndexedVector.hpp
coin-clp.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPackedVector.hpp
lps.o: lp-msg.h lp-types.h lp-sock.h coin-clp.h
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiClpSolverInterface.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpSimplex.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpModel.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpConfig.h
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/config_clp.h
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpPackedMatrix.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPragma.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpMatrixBase.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinFinite.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinUtilsConfig.h
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/config_coinutils.h
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPackedMatrix.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinError.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinPackedVectorBase.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinFloatEqual.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinShallowPackedVector.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinMessageHandler.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinHelperFunctions.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpParameters.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpObjective.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpSolve.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpLinearObjective.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiSolverInterface.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiCollections.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/OsiSolverParameters.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinWarmStartBasis.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinSort.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinDistance.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinWarmStart.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpEventHandler.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/ClpNode.hpp
lps.o: /home/chs/Documents/Prolog/mercury/lp-server/coin-Osi/dist/include/coin/CoinIndexedVector.hpp
