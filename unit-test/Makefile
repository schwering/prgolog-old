GOAL = suite
SOURCE_FILES = $(shell ls *.m)

EXE_MCFLAGS += --ml prgolog

all: enable-tests mercury-depends exe disable-tests

enable-tests:
	./enable-tests.sh

disable-tests:
	./disable-tests.sh

include ../Make.common

