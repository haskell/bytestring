GHC=            ghc
PKG=            -package fps

#GHC=            /usr/obj/build/compiler/stage2/ghc-inplace
#PKG=            

BIN=            run-utests run-qtests

GM4=             gm4
PP=             ./logpp
PP_OPTS=        -pgmF "$(PP)" -F

GHC_ = $(GHC) -threaded $(PP_OPTS) -fglasgow-exts --make

SRCS= TestFramework.hs UnitTestsMain.hs Tests.hs Quick.hs

all: qc htest bench sbench wc spellcheck letter_frequency linesort fuse

# build executable, linking in all tests
run-utests: $(SRCS)
	GM4=$(GM4) $(GHC_) -o $@ UnitTestsMain.hs

htest:: run-utests
	./run-utests

# quick testing
.PHONY: qc
qc::
	runhaskell Quick.hs

# attempt to benchmark small strings performance
sbench: 
	${GHC} ${PKG} -O -funbox-strict-fields -ddump-simpl-stats Quick.hs -o qc -no-recomp -package QuickCheck
	/usr/bin/time ./qc

bench::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} --make -O -funbox-strict-fields Bench.hs -o bench -no-recomp
	./bench +RTS -H64m

prof::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} -prof -auto-all --make -O -funbox-strict-fields Bench.hs -o bench
	./bench +RTS -H64m -p

.PHONY: wc
wc: wc.o
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	/usr/bin/time ./wc < bigdata
    
wc.o: wc.hs
	${GHC} ${PKG} -O -ddump-simpl-stats wc.hs -o wc

.PHONY: spellcheck
spellcheck: spellcheck.o
	/usr/bin/time ./spellcheck < spellcheck-input.txt
    
spellcheck.o: spellcheck.hs
	${GHC} ${PKG} -O $< -o spellcheck

.PHONY: letter_frequency
letter_frequency: letter_frequency.o
	/usr/bin/time ./letter_frequency < Usr.Dict.Words

letter_frequency.o: letter_frequency.hs
	${GHC} ${PKG} -O $< -o letter_frequency -ddump-simpl-stats

.PHONY: linesort
linesort: linesort.o
	/usr/bin/time ./linesort < Usr.Dict.Words

linesort.o: linesort.hs
	${GHC} ${PKG} -O $< -o linesort

.PHONY: fuse
fuse: fuse.o
	/usr/bin/time ./fuse < Usr.Dict.Words

fuse.o: fuse.hs
	${GHC} ${PKG} -O $< -o fuse -ddump-simpl-stats

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) bench */*.o */*.hi */*~ wc letter_frequency spellcheck linesort fuse
