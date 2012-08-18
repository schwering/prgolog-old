GOAL = cars_server
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml domain --ml planrecog --ml prgolog --ml visual

all: mercury-depends exe replay

replay: replay.c
	$(CC) -Wall -I../planrecog replay.c -o replay

include ../Make.common

