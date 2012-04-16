GOAL = visual
SOURCE_FILES = $(shell ls *.m)
NCURSES_NAME = ncurses-5.9
NCURSES = $(shell pwd)/$(NCURSES_NAME)

LIB_MCFLAGS += --ml prgolog --ml planrecog --ld-libflags -Wl,--no-as-needed -I${NCURSES}/include -L${NCURSES}/dist/lib -R${NCURSES}/dist/lib -lncurses -lpanel
EXE_MCFLAGS += --ml prgolog --ml planrecog --ld-libflags -Wl,--no-as-needed -I${NCURSES}/include -L${NCURSES}/dist/lib -R${NCURSES}/dist/lib -lpanel -lncurses

all: lib

exe lib: ncurses *.m planrecog

planrecog:
	make -C ../planrecog

ncurses:
	./install_ncurses.sh "$(NCURSES_NAME)" "$(MERCURY_SRC)" "$(MERCURY_INST)"

include ../Make.common

