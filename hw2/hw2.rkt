;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))

;;
;; Problem 1 The function iterator returns a function which when repeatedly called, returns the numbers in the sequence 
;;(range (list start step end)). When the sequence is exhausted, the returned function should return ().
;; iterator takes a list that has a start step and end elements and steps through starting at step by steps till end
;; it only retuns one value at a time so you need to use let to dynamicly change those values that at then
;; passed as alist that way when you repeatly call iterator the value of the argument can change
;;

(define (iterator rlist)
	(lambda ()
		(let
			((start (car rlist))
			(step (cadr rlist))
			(end (caddr rlist)))
			(if (> start end)
				'()
				(begin (set! rlist (list (+ start step) step end))
				start)))))



;;
;; Problem 2 Modify the seq function you wrote for the previous assignment so that it returns a stream rather than a list.
;;takes in a list that has 3 elements of start step end
;;it checks if start is greater than end if so returns empty stream
;; next it cons the elements into a stream and recurively calls the stream-seq with start being added by a step

	      (define (stream-seq f rlist)
(let (
	(start (car rlist)) 
	(step (cadr rlist))
 	(end (caddr rlist)) 
	)
(if 
	(> start end)
	empty-stream
	(stream-cons   (f start) (stream-seq f (list (+ start step) step end)))
)
)
)


;;
;; Problem 3 The scan function scan takes a binary function f, a value z, and a list l, and returns the list 
;; scan takes in a funtion value and list
;; if the list is null it returns the value as a list
;; else it cons the vaule to the list
;; next it recurviely calls scan with the function being used on the value and the first element of the list
;; the list is reduced by using cdr list so it will reach a base case of null

(define (scan f z l)
(if (null? l)
(list z)
( cons z (scan f (f z (car l)) (cdr l)))))


;;
;; Problem 4 does scan but with a stream not a list
;; stream scan is the same as scream but it takes a stream and return a stream
;; so all list functions are converted into steam function
;; checks if the stream is empty if so returns a stream of the value
;; next it cons the stream with the value 
;; then recurviely calls stream-scan with the function being applied to the vaule and first elemnt of the stream
;; then it reduces the stream with stream-rest so it reachs the base case of an empty stream

(define (stream-scan f z l)
(if (stream-empty? l)
(stream z)
( stream-cons z (stream-scan f (f z (stream-first l)) (stream-rest l)))))



;;
;; Problem 5 checks if a name is equal to the first element of a pair in a list and returns that pair
;; checks if the list is emtpy
;; returns an empty list
;; checks if the name is equal to the first element of the pair
;; returns the pair
;; recurviely calls lookup with the list being reduced by cdr

	(define (lookup name alist)
(if (null? alist)
	'()
	(if (eq? name (car (car alist))) 
		(car alist) 
		( lookup name (cdr alist)))))
;;
;; Problem 6 which returns the binding with the specified name in the nearest scope. If no such binding is found, return nil.
;; if the enb is null it returns an empty list
;; sets up a result using let thats the first par in an env and checks with look up
;; if the result is null then lookup-env is called recurviely with env being reduced with cdr to the base case
;; else the result is returned

				 
(define (lookup-env name env)
	(if (null? env)	
		'()
		(let 
			((result (lookup name (car env))))
		
			(if (null? result)
				(lookup-env name (cdr env))
				result))))
				
				
;;
;; Problem 7: Homework Statistics
;; problem 1: 40 mins
;; problem 2: 30 mins
;; problem 3 : 50 mins
;; problem 4 : 30 mins
;; problem 5 : 40 mins
;; problem 6 : 30 mins
