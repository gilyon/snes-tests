all:
	$(MAKE) -C cputest
	$(MAKE) -C spctest

package: all
	zip -r snes-tests.zip * -x "*.o" "*.spc"
