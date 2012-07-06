GOAL = cars_server
SOURCE_FILES = $(shell ls *.m)

# XXX Why is prgolog needed???
EXE_MCFLAGS += --ml bats --ml planrecog --ml prgolog --ml visual

all: exe replay

exe: *.m bats planrecog visual

replay: replay.c
	$(CC) -Wall -I../planrecog replay.c -o replay

bats:
	make -C ../bats

planrecog:
	make -C ../planrecog

visual:
	make -C ../visual

include ../Make.common

