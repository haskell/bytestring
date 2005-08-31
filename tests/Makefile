GHC=            ghc

BIN=            run-utests run-qtests

GM4=             m4
PP=             ./logpp
PP_OPTS=        -pgmF "$(PP)" -F

GHC_ = $(GHC) -threaded $(PP_OPTS) -fglasgow-exts --make

SRCS= TestFramework.hs UnitTestsMain.hs Tests.hs Quick.hs

all: run-utests

# build executable, linking in all tests
run-utests: $(SRCS)
	$(GHC_) -o $@ UnitTestsMain.hs

run:: run-utests
	./run-utests
	runhaskell Quick.hs

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) */*.o */*.hi */*~
