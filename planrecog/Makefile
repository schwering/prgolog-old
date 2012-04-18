GOAL = planrecog
SOURCE_FILES = $(shell ls *.m *.h)

LIB_MCFLAGS += --extra-library-header obs_types.h --ml prgolog --ml osi

all: lib

lib: *.m osi prgolog

osi:
	make -C ../osi

prgolog:
	make -C ../prgolog

include ../Make.common

