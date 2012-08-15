GOAL = bats
SOURCE_FILES = $(shell ls *.m)

LIB_MCFLAGS += --ml prgolog

all: lib

lib: mercury-depends *.m

include ../Make.common

