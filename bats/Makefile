GOAL = bats
SOURCE_FILES = $(shell ls *.m)

LIB_MCFLAGS += --ml prgolog

all: lib

lib: *.m prgolog

prgolog:
	make -C ../prgolog

include ../Make.common

