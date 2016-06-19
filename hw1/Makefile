.PHONY : all
all : hw1

.PHONY : clean
clean :
	rm -rf hw1 test compiled

hw1 : hw1.rkt
	raco exe -o $@ $<

test : test.rkt hw1.rkt
	raco exe -o $@ $<

.PHONY : run-tests
run-tests : test
	./test
