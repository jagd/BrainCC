.PHONY: all clean

COMPILER=ghc
FLAGS=--make -O2

all: TestBSM

TestBSM: TestBSM.hs BSM.hs BFI.hs
	$(COMPILER) $(FLAGS) $<

doc: *.hs
	haddock -h -o doc $^
	touch doc

clean:
	-rm *.o
	-rm *.hi
	-rm TestBSM
