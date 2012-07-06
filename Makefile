all:
	make -C util
	make -C osi
	make -C prgolog
	make -C maze
	make -C bats
	make -C planrecog
	make -C cars-main
	make -C cars-server
	make -C lp-server

clean:
	make -C util clean
	make -C osi clean
	make -C prgolog clean
	make -C maze clean
	make -C bats clean
	make -C planrecog clean
	make -C cars-main clean
	make -C cars-server clean
	make -C lp-server clean
	rm -rf lib

