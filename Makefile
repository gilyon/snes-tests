all:
	$(MAKE) -C cputest
	$(MAKE) -C spctest

package: all
	rm -f snes-tests.zip
	zip -r snes-tests.zip * -x "*.o" "*.spc"
