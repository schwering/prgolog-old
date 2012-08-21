GOAL = visual
SOURCE_FILES = $(shell ls *.m)
NCURSES_NAME = ncurses-5.9
NCURSES = $(shell pwd)/$(NCURSES_NAME)

LIB_MCFLAGS += --ml domain --ml prgolog --ml planrecog --ld-libflags -Wl,--no-as-needed -I${NCURSES}/include -L${NCURSES}/dist/lib -R${NCURSES}/dist/lib -lncurses -lpanel

all: mercury-depends ncurses lib

ncurses:
	./install_ncurses.sh "$(NCURSES_NAME)" "$(MERCURY_SRC)" "$(MERCURY_INST)"

include ../Make.common

