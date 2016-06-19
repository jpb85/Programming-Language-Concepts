#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "hw1.rkt")

(define hw1-tests
  (test-suite
   "Homework 1 Tests"
   (check-equal? (deep-count '(1 (2 3) (4)))
                 4
                 "(deep-count '(1 (2 3) (4)))")
   (check-equal? (reverse '(1 (2 3) 4 5))
                 '(5 4 (2 3) 1)
                 "(reverse '(1 (2 3) 4 5))")
   (check-equal? (range '(0 2 7))
                 '(0 2 4 6)
                 "(range '(0 2 7))")
   (check-equal? (range '(2 2 0))
                 '()
                 "(range '(2 2 0))")
   (check-equal? (seq (lambda (x) (* x x)) '(0 2 7))
                 '(0 4 16 36)
                 "(seq (lambda (x) (* x x)) '(0 2 7))")
))

(run-tests hw1-tests)
