GOAL = cars
SOURCE_FILES = $(shell ls *.m *.h)

EXE_MCFLAGS += --ml prgolog --ml osi --ml util

all: exe replay

exe: osi prgolog util

prgolog:
	make -C ../prgolog

util:
	make -C ../util

osi:
	make -C ../osi

include ../Make.common

replay: replay.c
	cc -Wall -O3 -o replay replay.c

