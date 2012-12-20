GOAL = domain
SOURCE_FILES = $(shell ls *.m *.h)

LIB_MCFLAGS += --extra-library-header domain-car-obs-torcs-types.h --ml prgolog --ml util

all: mercury-depends lib

include ../Make.common

