GOAL = visual
SOURCE_FILES = $(shell ls *.m)

LIB_MCFLAGS += --ml prgolog --ml planrecog -lncurses -lpanel
EXE_MCFLAGS += --ml prgolog --ml planrecog -lncurses -lpanel

all: exe

lib: *.m planrecog

planrecog:
	make -C ../planrecog

include ../Make.common

