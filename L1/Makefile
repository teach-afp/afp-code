sl : SpaceLeaks
	time ./SpaceLeaks +RTS -s

SpaceLeaks : SpaceLeaks.hs Makefile
	ghc -main-is SpaceLeaks SpaceLeaks.hs

io : LazyIO
	./LazyIO

LazyIO : LazyIO.hs Makefile
	ghc -main-is LazyIO LazyIO.hs

files:
	mkdir files
	for i in $$(seq 1 10000); do echo $$i > files/$$i.txt; done
