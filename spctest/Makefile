TEST_ASMS = spc_tests0.asm spc_tests1.asm spc_tests2.asm
TEST_SPCS = $(patsubst %.asm,%.spc,$(TEST_ASMS))

all: spctest.sfc

%.spc: %.asm spc_common.inc
	spcasm -f plain $< $@

spctest.o: spctest.asm $(TEST_SPCS) font.bin
	ca65 spctest.asm -o spctest.o

$(TEST_ASMS) tests.txt: make_tests.py
	python3 make_tests.py

spctest.sfc: spctest.o lorom128.cfg
	ld65 -C lorom128.cfg -o spctest.sfc spctest.o

clean:
	rm -f spctest.o spctest.spc $(TEST_ASMS)
