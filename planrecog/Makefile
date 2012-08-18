GOAL = planrecog
SOURCE_FILES = $(shell ls *.m)

LIB_MCFLAGS += --ml prgolog --ml prbat

all: lib

lib: mercury-depends *.m

include ../Make.common

