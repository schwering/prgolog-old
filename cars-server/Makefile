GOAL = prs
SOURCE_FILES = $(shell ls *.m *.h)

EXE_MCFLAGS += --ml planrecog

all: exe

exe: *.m planrecog

planrecog:
	make -C ../planrecog

include ../Make.common

