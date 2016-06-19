#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "mceval.rkt")

(define (test-mceval exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (mceval exp (setup-environment))))

(define (test-mceval-exception exp)
  (mceval exp (setup-environment)))

(define primitive-tests
  (test-suite
   "Test primitives"
   (check-equal? (test-mceval '(+ 4 5))
                 9
                 "Implement +")
   (check-equal? (test-mceval '(* 4 5))
                 20
                 "Implement *")
   (check-equal? (test-mceval '(- 4 5))
                 -1
                 "Implement -")
   (check-equal? (test-mceval '(/ 8 4))
                 2
                 "Implement /")
   (check-equal? (test-mceval '(< 4 4))
                 #f
                 "Implement <")
   (check-equal? (test-mceval '(= 4 4))
                 #t
                 "Implement =")
   (check-equal? (test-mceval '(>= 4 4))
                 #t
                 "Implement >=")
   (check-equal? (test-mceval '(> 4 4))
                 #f
                 "Implement >")
   (check-exn (regexp "^Metacircular Interpreter Aborted$")
              (lambda () (test-mceval-exception '(error)))
              "Implement error")))

(define and-tests
  (test-suite
   "Test the and special forms"
   (check-equal? (test-mceval '(and (= 2 2) (> 2 1)))
                 #t
                 "(and (= 2 2) (> 2 1))")
   (check-equal? (test-mceval '(and (= 2 2) (< 2 1)))
                 #f
                 "(= 2 2) (< 2 1))")
   (check-equal? (test-mceval '(and 1 2 'c '(f g)))
                 '(f g)
                 "(and 1 2 'c '(f g)))")
   (check-equal? (test-mceval '(and false (error)))
                 #f
                 "(and 1 2 'c '(f g)))")
   (check-equal? (test-mceval '(and))
                 #t
                 "(and)")))

(define or-tests
  (test-suite
   "Test the or special forms"
   (check-equal? (test-mceval '(or (= 2 2) (> 2 1)))
                 #t
                 "(or (= 2 2) (> 2 1))")
   (check-equal? (test-mceval '(or (= 2 2) (< 2 1)))
                 #t
                 "(or (= 2 2) (< 2 1))")
   (check-equal? (test-mceval '(or false false false))
                 #f
                 "(or false false false)")
   (check-equal? (test-mceval '(or true (error)))
                 #t
                 "(or true (error))")
   (check-equal? (test-mceval '(or))
                 #f
                 "(or)")))

(define let-tests
  (test-suite
   "Test the let special forms"
   (check-equal? (test-mceval '(let ((x 1) (y 2)) (+ x y)))
                 3
                 "(let ((x 1) (y 2)) (+ x y))")))

(define delay-force-tests
  (test-suite
   "Test delay and force"
   (check-equal? (test-mceval '(begin (delay (error)) 3))
                 3
                 "(begin (delay (error)) 3)")
   (check-equal? (test-mceval '(force (delay 3)))
                 3
                 "(force (delay 3))")
   (check-equal? (test-mceval '((delay 5)))
                 5
                 "((delay 5))")
   (check-equal? (test-mceval '(let ((x (delay 3))) (force x)))
                 3
                 "(let ((x (delay 3))) (force x))")
   (check-equal? (test-mceval '(force (delay (force (delay 3)))))
                 3
                 "(force (delay (force (delay 3))))")
   (check-equal? (test-mceval '(let ((x (delay (+ 1 2)))) (+ (force x) (force x))))
                 6
                 "(let ((x (delay (+ 1 2)))) (+ (force x) (force x)))")
   (check-equal? (test-mceval '(let ((x 0))
                                 (let ((y (delay (begin (set! x (+ x 1)) x))))
                                   (+ (force y) (force y)))))
                 2
                 "Delayed expression with side-effect")))

(define stream-tests
  (test-suite
   "Test streams"
   (check-equal? (test-mceval '(stream-empty? empty-stream))
                 #t
                 "(stream-empty? empty-stream)")
   (check-equal? (test-mceval '(stream-first (stream-cons 1 empty-stream)))
                 1
                 "(stream-first (stream-cons 1 empty-stream))")
   (check-equal? (test-mceval '(stream-empty? (stream-rest (stream-cons 1 empty-stream))))
                 #t
                 "(stream-empty? (stream-rest (stream-cons 1 empty-stream)))")
   (check-equal? (test-mceval '(stream-first (stream-cons 1 (error))))
                 1
                 "(stream-first (stream-cons 1 (error)))")
   (check-equal? (test-mceval '(stream-first (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error)))))
                 5
                 "(stream-first (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error))))")
   (check-equal? (test-mceval '(stream-first (stream-rest (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error))))))
                 5
                 "(stream-first (stream-rest (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error)))))")))
  
(run-tests primitive-tests)
(run-tests and-tests)
(run-tests or-tests)
(run-tests let-tests)
(run-tests delay-force-tests)
(run-tests stream-tests)
