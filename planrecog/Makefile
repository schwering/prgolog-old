GOAL = planrecog
SOURCE_FILES = $(shell ls *.m *.h)

LIB_MCFLAGS += --extra-library-header obs_types.h --ml prgolog --ml bats

all: lib

lib: mercury-depends *.m

include ../Make.common

