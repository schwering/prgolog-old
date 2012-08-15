GOAL = cars
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml bats --ml planrecog --ml prgolog --ml util

all: exe replay

exe: mercury-depends *.m

include ../Make.common

replay: replay.c
	cc -Wall -O3 -o replay replay.c

