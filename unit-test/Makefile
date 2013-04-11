GOAL = suite
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml prgolog --ml domain --ml util

#all: enable-tests mercury-depends exe disable-tests

all: mercury-depends exe

enable-tests:
	./enable-tests.sh

disable-tests:
	./disable-tests.sh

include ../Make.common

