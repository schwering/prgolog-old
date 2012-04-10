GOAL = planrecog
SOURCE_FILES = $(shell ls *.m)

LIB_MCFLAGS += --ml prgolog --ml osi --ml util

all: lib

lib: *.m osi prgolog util

osi:
	make -C ../osi

prgolog:
	make -C ../prgolog

util:
	make -C ../util

include ../Make.common

