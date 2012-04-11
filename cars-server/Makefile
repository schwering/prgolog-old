GOAL = prs

all: planrecog prs

include ../Make.common

MERCURY_LINKAGE = --mercury-linkage shared

CFLAGS   = $(shell $(MMC) $(MERCURY_LINKAGE) $(LIB_MCFLAGS) --ml --grade $(GRADE) --output-cflags) -Wall
CXXFLAGS = $(CFLAGS) -fpermissive
LDFLAGS  = $(shell $(MMC) $(MERCURY_LINKAGE) $(LIB_MCFLAGS) --ml planrecog --output-library-link-flags)

planrecog:
	make -C ../prgolog

prs: mercury_interface.o prs.o
	$(CXX) $(LDFLAGS) $^ -o $@

mercury_interface.c mercury_interface.h:
	$(MMC) $(LIB_MCFLAGS) --ml planrecog --generate-standalone-interface mercury_interface


