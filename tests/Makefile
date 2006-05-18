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

all: qc prop sbench sbench2 
everything: qc prop sbench sbench2 runbench htest wc spellcheck letter_frequency linesort fuse inline

# build executable, linking in all tests
run-utests: $(SRCS)
	GM4=$(GM4) $(GHC_) -o $@ UnitTestsMain.hs

htest:: run-utests
	./run-utests

# quick testing
.PHONY: qc
qc::
	runhaskell Quick.hs

# more testing
.PHONY: prop
prop::
	runhaskell Properties.hs

# attempt to benchmark small strings performance
sbench: 
	${GHC} ${PKG} --make -O2 -optc-O2 -funbox-strict-fields Quick.hs -o qc -no-recomp -package QuickCheck
	/usr/bin/time ./qc

# attempt to benchmark lazy strings performance
sbench2: 
	${GHC} ${PKG} --make -O2 -optc-O2 -funbox-strict-fields Properties.hs -o lqc -no-recomp -package QuickCheck
	/usr/bin/time ./lqc

.PHONY: runbench
runbench: bench
	./bench +RTS -H64m

bench: Bench.hs
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} --make -O -funbox-strict-fields Bench.hs -o bench -no-recomp

prof::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} -prof -auto-all --make -O -funbox-strict-fields Bench.hs -o bench
	./bench +RTS -H64m -p

wc: wc.o
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	/usr/bin/time ./wc < bigdata
wc.o: wc.hs
	${GHC} ${PKG} -O -ddump-simpl-stats wc.hs -o wc

spellcheck: spellcheck.o
	/usr/bin/time ./spellcheck < spellcheck-input.txt
spellcheck.o: spellcheck.hs
	${GHC} ${PKG} -O $< -o spellcheck

letter_frequency: letter_frequency.o
	/usr/bin/time ./letter_frequency < Usr.Dict.Words
letter_frequency.o: letter_frequency.hs
	${GHC} ${PKG} -O $< -o letter_frequency -ddump-simpl-stats

linesort: linesort.o
	/usr/bin/time ./linesort < Usr.Dict.Words
linesort.o: linesort.hs
	${GHC} ${PKG} -O $< -o linesort

fuse: fuse.o
	/usr/bin/time ./fuse < Usr.Dict.Words
fuse.o: fuse.hs
	${GHC} ${PKG} -O $< -o fuse -ddump-simpl-stats

lazylines: lazylines.o
	/usr/bin/time ./lazylines < bigdata
lazylines.o: lazylines.hs
	${GHC} ${PKG} -O $< -o lazylines

inline: inline.o
	/usr/bin/time ./inline
inline.o: inline.hs
	${GHC} ${PKG} -O $< -o inline

results: bench
	./bench +RTS -H64m > results

.PHONY: plot
plot:: results
	echo -e 'set terminal png          \n \
             set output "results.png"  \n \
             set yrange [0:2]          \n \
             plot "results" using ($$2) with linespoints, "" using ($$3) with linespoint' |\
             gnuplot ;
	xv results.png    

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) bench */*.o */*.hi */*~ wc letter_frequency spellcheck linesort fuse inline lqc qc
