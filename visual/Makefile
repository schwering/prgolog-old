GOAL = visual
SOURCE_FILES = $(shell ls *.m)
NCURSES_NAME = ncurses-5.9
NCURSES = $(shell pwd)/$(NCURSES_NAME)

LIB_MCFLAGS += --ml bats --ml prgolog --ml planrecog --ml util --ld-libflags -Wl,--no-as-needed -I${NCURSES}/include -L${NCURSES}/dist/lib -R${NCURSES}/dist/lib -lncurses -lpanel

all: lib

exe lib: ncurses *.m bats planrecog util

bats:
	make -C ../bats

prgolog:
	make -C ../prgolog

planrecog:
	make -C ../planrecog

util:
	make -C ../util

ncurses:
	./install_ncurses.sh "$(NCURSES_NAME)" "$(MERCURY_SRC)" "$(MERCURY_INST)"

include ../Make.common

