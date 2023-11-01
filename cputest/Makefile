all: full basic

full: cputest-full.sfc

basic: cputest-basic.sfc


tests-full.inc tests-full.txt: make_cpu_tests.py
	python3 make_cpu_tests.py

tests-basic.inc tests-basic.txt: make_cpu_tests.py
	python3 make_cpu_tests.py --basic

cputest-full.o: main.asm tests-full.inc font.bin
	ca65 main.asm -o cputest-full.o

cputest-basic.o: main.asm tests-basic.inc font.bin
	ca65 -D basic main.asm -o cputest-basic.o

cputest-full.sfc: cputest-full.o lorom.cfg
	ld65 -C lorom.cfg -o cputest-full.sfc cputest-full.o -m tests-full.map -vm

cputest-basic.sfc: cputest-basic.o lorom.cfg
	ld65 -C lorom.cfg -o cputest-basic.sfc cputest-basic.o -m tests-basic.map -vm
