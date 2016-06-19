CABAL ?= cabal
GHC ?= ghc

.PHONY : all
all : hw5

.PHONY : create-sandbox
create-sandbox :
	$(CABAL) update
	$(CABAL) sandbox init
	$(CABAL) install --dependencies-only

.PHONY : clean
clean :
	$(CABAL) clean
	rm -f hw5

.PHONY : dist-clean
dist-clean : clean
	rm -rf cabal.sandbox.config .cabal-sandbox

hw5 : dist/build/hw5/hw5
	cp $< $@

dist/build/hw5/hw5 : cabal.sandbox.config
	$(CABAL) build

cabal.sandbox.config : create-sandbox

.PHONY : run-tests
run-tests : hw5
	./hw5 --timeout=2
