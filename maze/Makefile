GOAL = maze
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml prgolog

all: exe

exe: *.m prgolog

prgolog:
	make -C ../prgolog

include ../Make.common

