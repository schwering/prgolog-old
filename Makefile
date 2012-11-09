DIRS = cars-server cars-main domain lp-server osi planrecog prgolog util visual unit-test
MAINS = $(shell find $(DIRS) -maxdepth 1 -name \*.m -or -name \*.c -or -name \*.cc | xargs grep -l '\(^:- pred main\|int main\)' | sed -e 's/\/.\+$$//g' | uniq)

all:
	$(foreach MAIN,$(MAINS), make -C $(MAIN) || exit; )

clean:
	$(foreach DIR,$(DIRS), make -C $(DIR) clean || exit; )
	rm -rf lib

depend:
	./make-depend.sh $(DIRS)

