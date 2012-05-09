COIN = $(shell pwd)/coin-Osi
GOAL = lp_server
HEADER_FILES = $(shell ls *.h)
BODY_FILES = $(shell ls *.cc)
SOURCE_FILES = $(HEADER_FILES) $(BODY_FILES)
OBJECT_FILES = $(BODY_FILES:%.cc=%.o)

CXXFLAGS = -Wall
CPPFLAGS = -I$(COIN)/dist/include/coin
LD	 = g++
LDFLAGS	 = -Wl,--no-as-needed -Wl,-rpath=$(COIN)/dist/lib -L$(COIN)/dist/lib -lClp -lOsi -lOsiClp -lCoinUtils -lblas -lm -lpthread

DEPEND          = makedepend -f-
CDEPEND         = $(DEPEND) -Y. -- $(CPPFLAGS) --
CXXDEPEND       = $(DEPEND) -Y. -- $(CPPFLAGS) --
DEPEND_FILE	= .depends

all: $(COIN)/dist exe

exe: $(OBJECT_FILES)
	$(LD) $(LDFLAGS) -o $(GOAL) $(OBJECT_FILES)

$(COIN):
	test -d "$(COIN)" || (svn co https://projects.coin-or.org/svn/Osi/stable/0.102 "$(COIN)" || exit 1)

$(COIN)/dist: $(COIN)
	cd "$(COIN)" ;\
	./configure "--prefix=$(COIN)/dist" ;\
	make ;\
	make install ;\
	cd ..

depend: $(SOURCE_FILES)
	$(CXXDEPEND) $(SOURCE_FILES) >$(DEPEND_FILE)

clean:
	rm -f *.o
	rm -f $(GOAL)

-include $(DEPEND_FILE)

