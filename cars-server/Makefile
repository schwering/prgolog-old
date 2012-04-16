GOAL = prs
SOURCE_FILES = $(shell ls *.m)

# XXX Why is prgolog needed???
EXE_MCFLAGS += --ml planrecog --ml prgolog --ml visual

all: exe replay

exe: *.m planrecog visual

replay: replay.c
	$(CC) -Wall -I../planrecog replay.c -o replay

planrecog:
	make -C ../planrecog

visual:
	make -C ../visual

include ../Make.common

