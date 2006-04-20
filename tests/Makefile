GHC=            ghc

BIN=            run-utests run-qtests

GM4=             gm4
PP=             ./logpp
PP_OPTS=        -pgmF "$(PP)" -F

GHC_ = $(GHC) -threaded $(PP_OPTS) -fglasgow-exts --make

SRCS= TestFramework.hs UnitTestsMain.hs Tests.hs Quick.hs

all: run bench

# build executable, linking in all tests
run-utests: $(SRCS)
	GM4=$(GM4) $(GHC_) -o $@ UnitTestsMain.hs

run:: run-utests
	./run-utests
	runhaskell Quick.hs

bench::
	if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	ghc --make -O -funbox-strict-fields Bench.hs -o bench -package fps
	./bench +RTS -H64m

prof::
	if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	ghc -prof -auto-all --make -O -funbox-strict-fields Bench.hs -o bench -package fps
	./bench +RTS -H64m -p

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) bench */*.o */*.hi */*~
