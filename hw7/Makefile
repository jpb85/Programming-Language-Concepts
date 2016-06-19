CABAL ?= cabal
GHC ?= ghc
DOT ?= dot

.PHONY : all
all : cabal.sandbox.config
	$(CABAL) install --disable-documentation
	cp .cabal-sandbox/bin/fsm* .
	cp .cabal-sandbox/bin/test* .

~/.cabal/config :
	$(CABAL) update

cabal.sandbox.config : ~/.cabal/config
	$(CABAL) update
	$(CABAL) sandbox init
	$(CABAL) install --dependencies-only

.PHONY : clean
clean :
	$(CABAL) clean
	rm -f fsm test match match.c *.dot *.pdf

.PHONY : distclean
distclean : clean
	rm -rf cabal.sandbox.config .cabal-sandbox

.PHONY : run-tests
run-tests : all
	./test

.PHONY : figs
figs : epsilon.pdf a.pdf ab.pdf a-or-b.pdf ab-star.pdf

test : dist/build/test/test
	cp $< $@

match : main.c match.c
	gcc -o $@ $^

%.pdf : %.dot
	$(DOT) -Tpdf -o $@ $< || rm -f $@

%-nfa.dot %-dfa.dot %-min-dfa.dot : %.txt fsm
	./fsm --nfa-dot=$*-nfa.dot --dfa-dot=$*-dfa.dot --min-dfa-dot=$*-min-dfa.dot $(shell cat $<)

epsilon.dot : fsm
	./fsm --nfa-dot=$@ ""

a.dot : fsm
	./fsm --nfa-dot=$@ "a"

ab.dot : fsm
	./fsm --nfa-dot=$@ "ab"

a-or-b.dot : fsm
	./fsm --nfa-dot=$@ "a|b"

ab-star.dot : fsm
	./fsm --nfa-dot=$@ "(ab)*"
