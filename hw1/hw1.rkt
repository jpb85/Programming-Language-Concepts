;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))

;;
;; Problem 1
;;check if null then zero
;;then checks if the first part of the list is a pair
;;recuresivly breaks up list from rest and first
;;counts only the atoms by check if not pair and recuresivly calls back the rest of the list
;;




;;
;; Problem 2
;;reserve starts it by having the list and the empty list to be reved
;;the rhelper reverses the list to be reversed, and the reversed list
;;by taking the rest and then cons the first part of the list 
;;and adding that to the second list




(define (reverse l)
  (rhelper l '()))

(define (rhelper l lst2)
  (if (null? l)
      lst2
      (rhelper (cdr l) (cons (car l) lst2))))
	


;;
;; Problem 3
;;range sets the start as the first element,step is the second element
;;end is the last element
;;it cheks if start is bigger than end
;;if it isnt then it build a list with the start
;;and recurvisely calls range the the step added

  (define (range rlist)
	(let (
	(start (car rlist)) 
	(step (car (cdr rlist)))
 	(end (car (cdr (cdr rlist)))) 
	)
	(if 
	(> start end)
	'()
	(cons start (range (list (+ start step) step end)))
	)
	)
	)

;;
;; Problem 4
;;
;;seq sets the start as the first element,step is the second element
;;end is the last element
;;it cheks if start is bigger than end
;;if it isnt then it build a list with the start
;;and recurvisely calls seq the the function added

      (define (seq f rlist)
(let (
	(start (car rlist)) 
	(step (car (cdr rlist)))
 	(end (car (cdr (cdr rlist)))) 
	)
(if 
	(> start end)
	'()
	(cons   (f start) (seq f (list (+ start step) step end)))
)
)
)
;;
;; Problem 5: Homework Statistics
;; problem1 - 30 mins
;; problem2 - 30 mins
;; problem3 - 35 mins
;; problem 4 -30 mins
