GHC=            ghc
PKG=            -package fps

#GHC=            /usr/obj/build/compiler/stage2/ghc-inplace
#PKG=            

BIN=            run-utests run-qtests

all:  prop prop-fusion-compiled prop-compiled prop-opt
fast: prop-fast

everything: prop hugs prop-compiled prop-fusion-compiled runbench fusionbench wc spellcheck letter_frequency linesort fuse down-fuse inline

# ---------------------------------------------
# QuickCheck

# interpreted
.PHONY: prop hugs
prop::
	@echo "Interpreted"	
	runhaskell Properties.hs

hugs::
	runhugs -98 Properties.hs

# interpreted fast. Just a quick check.
.PHONY: prop-fast
prop-fast::
	runhaskell Properties.hs 20

# compiled
.PHONY: prop-compiled
prop-compiled: 
	@echo "Compiled, rules off"	
	${GHC} ${PKG} --make -cpp -O2 -optc-O2 -frules-off -funbox-strict-fields Properties.hs -o p -no-recomp -package QuickCheck
	/usr/bin/time ./p

# rules-on
.PHONY: prop-opt
prop-opt: 
	@echo "Compiled, rules on"	
	${GHC} ${PKG} --make -cpp -O2 -optc-O2 -funbox-strict-fields Properties.hs -o p -no-recomp -package QuickCheck
	/usr/bin/time ./p

#
# test rewriting is correct and working.
#
.PHONY: prop-fusion-compiled
prop-fusion-compiled:
	${GHC} ${PKG} --make -cpp -O2 -optc-O2 -funbox-strict-fields FusionProperties.hs -o fp -no-recomp -package QuickCheck
	/usr/bin/time ./fp

# ---------------------------------------------------------
# Benchmarking

.PHONY: runbench
runbench: bench
	: ./bench +RTS -H64m

bench::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	: ${GHC} ${PKG} --make -O -funbox-strict-fields Bench.hs -cpp -o bench -no-recomp

fusionbench::
	${GHC} ${PKG} --make -O -funbox-strict-fields FusionBench.hs -cpp -ddump-simpl-stats -o fusionbench -no-recomp

# ---------------------------------------------------------
# Profiling
.PHONY: prof

prof::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} -prof -auto-all --make -O -funbox-strict-fields Bench.hs -o bench
	./bench +RTS -H64m -p

fusionprof::
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	${GHC} ${PKG} -prof -auto-all --make -O -funbox-strict-fields FusionBench.hs -o fusionbench
	./bench +RTS -H64m -p

# ---------------------------------------------------------
# Small programs

wc: wc.o
	@if [ ! -f "bigdata" ] ; then ln -s data bigdata ; fi
	/usr/bin/time ./wc bigdata
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

down-fuse: down-fuse.o
	/usr/bin/time ./down-fuse
down-fuse.o: down-fuse.hs
	${GHC} ${PKG} -O $< -o ./down-fuse -ddump-simpl-stats

lazylines: lazylines.o
	/usr/bin/time ./lazylines < bigdata
lazylines.o: lazylines.hs
	${GHC} ${PKG} -O $< -o lazylines

inline: inline.o
	/usr/bin/time ./inline
inline.o: inline.hs
	${GHC} ${PKG} -O $< -o inline

# ---------------------------------------------------
# Plotting graphs

results: bench
	./bench +RTS -H64m > results

.PHONY: plot
plot:: results
	echo -e 'set terminal png          \n \
             set output "results.png"  \n \
             set yrange [0:2]          \n \
             plot "results" using ($$1):($$2) with boxes, "" using ($$1):($$3) with boxes' |\
             gnuplot ;
	xv results.png    

# ---------------------------------------------------
# Cleaning

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) bench */*.o */*.hi */*~ 
	rm -f wc letter_frequency spellcheck linesort fuse inline p fp lazylines
	rm -f down-fuse
