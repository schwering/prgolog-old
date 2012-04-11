GOAL = cars
SOURCE_FILES = $(shell ls *.m *.h)

EXE_MCFLAGS += --ml planrecog --ml prgolog --ml osi --ml util

all: exe replay

exe: *.m planrecog prgolog osi util

planrecog:
	make -C ../planrecog

prgolog:
	make -C ../prgolog

osi:
	make -C ../osi

util:
	make -C ../util

include ../Make.common

replay: replay.c
	cc -Wall -O3 -o replay replay.c

