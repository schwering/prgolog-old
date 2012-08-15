GOAL = cars_server
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml bats --ml planrecog --ml prgolog --ml visual

all: exe replay

exe: mercury-depends *.m

replay: replay.c
	$(CC) -Wall -I../planrecog replay.c -o replay

include ../Make.common

