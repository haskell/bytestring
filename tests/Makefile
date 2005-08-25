GHC=            ghc

BIN=            run-utests

M4=             /usr/local/bin/gm4
PP=             ./logpp
PP_OPTS=        -pgmF "$(PP)" -F

GHC_ = $(GHC) -threaded $(PP_OPTS) -fglasgow-exts --make

SRCS= TestFramework.hs UnitTestsMain.hs Tests.hs

all: run-utests

# build executable, linking in all tests
run-utests: $(SRCS)
	$(GHC_) -o $@ UnitTestsMain.hs

.PHONY: clean
clean:
	rm -f *~ *.o *.hi $(BIN) */*.o */*.hi */*~
