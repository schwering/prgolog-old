all:
	make -C util
	make -C osi
	make -C prgolog
	make -C maze
	make -C cars

clean:
	make -C util clean
	make -C osi clean
	make -C prgolog clean
	make -C maze clean
	make -C cars clean
	rm -rf lib

