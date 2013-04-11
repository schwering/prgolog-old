GOAL = cars_server
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml domain --ml planrecog --ml prgolog --ml visual --ml util

all: mercury-depends exe replay

replay: replay.c
	$(CC) -Wall -I../domain replay.c -o replay

include ../Make.common

