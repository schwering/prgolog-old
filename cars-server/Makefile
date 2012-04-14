GOAL = prs
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml planrecog

all: exe replay

exe: *.m planrecog

replay: replay.c
	$(CC) -Wall -I../planrecog replay.c -o replay

planrecog:
	make -C ../planrecog

include ../Make.common

