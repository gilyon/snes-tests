all: cputest.sfc


tests.inc tests.txt: make_cpu_tests.py
	python3 make_cpu_tests.py

cputest.o: main.asm tests.inc font.bin
	ca65 main.asm -o cputest.o

cputest.sfc: cputest.o lorom.cfg
	ld65 -C lorom.cfg -o cputest.sfc cputest.o -m tests.map -vm

