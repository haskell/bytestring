GHC=            ghc

BIN=            run-utests run-qtests

GM4=             gm4
PP=             ./logpp
PP_OPTS=        -pgmF "$(PP)" -F

GHC_ = $(GHC) -threaded $(PP_OPTS) -fglasgow-exts --make

SRCS= TestFramework.hs UnitTestsMain.hs Tests.hs Quick.hs

all: qc htest bench wc spellcheck letter_frequency linesort

# build executable, linking in all tests
run-utests: $(SRCS)
	GM4=$(GM4) $(GHC_) -o $@ UnitTestsMain.hs

htest:: run-utests
	./run-utests

qc: Quick.hs
	runhaskell Quick.hs

bench::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	ghc -package fps --make -O -funbox-strict-fields Bench.hs -o bench -no-recomp
	./bench +RTS -H64m

prof::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	ghc -package fps -prof -auto-all --make -O -funbox-strict-fields Bench.hs -o bench
	./bench +RTS -H64m -p

.PHONY: wc
wc: wc.o
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	/usr/bin/time ./wc < bigdata
    
wc.o: wc.hs
	ghc -package fps -O wc.hs -o wc

.PHONY: spellcheck
spellcheck: spellcheck.o
	/usr/bin/time ./spellcheck < spellcheck-input.txt
    
spellcheck.o: spellcheck.hs
	ghc -package fps -O $< -o spellcheck

.PHONY: letter_frequency
letter_frequency: letter_frequency.o
	/usr/bin/time ./letter_frequency < Usr.Dict.Words

letter_frequency.o: letter_frequency.hs
	ghc -package fps -O $< -o letter_frequency

.PHONY: linesort
linesort: linesort.o
	/usr/bin/time ./linesort < Usr.Dict.Words

linesort.o: linesort.hs
	ghc -package fps -O $< -o linesort

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) bench */*.o */*.hi */*~ wc letter_frequency spellcheck linesort
