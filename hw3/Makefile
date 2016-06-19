RACOEXEFLAGS=--collects-path $(shell readlink -m $$(which racket)/../../share/racket/collects) --exf-clear ++exf -m ++exf -U ++exf --

.PHONY : all
all : mceval

.PHONY : clean
clean :
	rm -rf mceval test compiled

mceval : mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

test : test.rkt mceval.rkt
	raco exe -o $@ $<

.PHONY : run-tests
run-tests : test
	./test
