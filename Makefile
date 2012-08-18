DIRS = bats cars-main cars-server lp-server maze osi planrecog prgolog util visual
MAINS = $(shell find $(DIRS) -maxdepth 1 -name \*.m -or -name \*.c -or -name \*.cc | xargs grep -l '\(^:- pred main\|int main\)' | sed -e 's/\/.\+$$//g' | uniq)

all:
	$(foreach MAIN,$(MAINS), make -C $(MAIN); )

clean:
	$(foreach DIR,$(DIRS), make -C $(DIR) clean; )
	rm -rf lib

depend:
	./make-depends.sh $(DIRS)

