GOAL = cars
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml domain --ml planrecog --ml prgolog --ml util

all: mercury-depends exe replay

include ../Make.common

replay: replay.c
	cc -Wall -O3 -o replay replay.c

