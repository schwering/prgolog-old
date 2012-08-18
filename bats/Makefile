GOAL = prbat
SOURCE_FILES = $(shell ls *.m *.h)

LIB_MCFLAGS += --extra-library-header obs_types.h --ml prgolog

all: lib

lib: mercury-depends *.m

include ../Make.common

