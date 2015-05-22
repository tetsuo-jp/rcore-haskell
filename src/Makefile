all: main

warn: Main.hs
	ghc -Wall -fno-warn-unused-do-bind --make Main.hs

main:
	ghc --make Main.hs

clean:
	-rm -f Main
	-rm -f *.hi *.o
	-rm -f Core/*.hi Core/*.o
	-rm -f RCore/*.hi RCore/*.o

test:
	./Main ../examples/reverse.rcore ../examples/list123_atom.rwhile 
