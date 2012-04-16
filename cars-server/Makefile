GOAL = prs
SOURCE_FILES = $(shell ls *.m)

# XXX Why is prgolog needed???
EXE_MCFLAGS += --ml planrecog --ml prgolog

all: exe replay

exe: *.m planrecog

replay: replay.c
	$(CC) -Wall -I../planrecog replay.c -o replay

planrecog:
	make -C ../planrecog

include ../Make.common

