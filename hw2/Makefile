.PHONY : all
all : hw2

.PHONY : clean
clean :
	rm -rf hw2 test compiled

hw2 : hw2.rkt
	raco exe -o $@ $<

test : test.rkt hw2.rkt
	raco exe -o $@ $<

.PHONY : run-tests
run-tests : test
	./test
