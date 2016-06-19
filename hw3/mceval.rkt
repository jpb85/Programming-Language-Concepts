#lang racket
(require racket/mpair)
(require readline/readline)

;;;;Ported to Racket by Geoffrey Mainland <mainland@cs.drexel.edu>

;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;SECTION 4.1.1






;;this is the main function of and 
;;it takes the base case of null and return false
;; it sets an element which is the first expression of the expression which is the car 
 (define (eval-and exp env) 
	(if null? exp #t) ;;this is ment to be base case
			;;where it is only null so returns true	
			;;doesnt work for some reason idk
	

         (let ((element (mceval (first-exp exp) env))) 
		(if element '() #t) 	;;if element is emptpy list return true
                 (cond ((last-exp? exp)   ;;checks if this is the last exp
                            (if  element element #f)) ;;return the element if the element is a true value else it returns false
                           (else 
                                 (if element  ;;if the element is true keep evaluing the expressing
                                         (eval-and (rest-exps exp) env) 
                                         #f))))) ;;if its false break out and just do false
  



;;this is the main function of or 
;;it takes the base case of null and return false
;; it sets an element which is the first expression of the expression which is the car
 (define (eval-or exp env) 
	(if null? exp #f) ;;this is ment to be base case
			;;where it is only null so returns false	
			;;doesnt work for some reason idk
         (let ((element (mceval (first-exp exp) env))) 
                 (cond ((last-exp? exp) 
				(if element '() #f)
                            (if  element element #f)) 
                           (else 
                                 (if element 
                                     element 
                                     (eval-or (rest-exps exp) env)))))) 


;;define the case for and and checking if tagged-list
 (define (and? exp) (tagged-list? exp 'and)) 
;;this is helper function for getting the rest of the and arguments
 (define (and-cdr exp) (cdr exp)) 
 
 ;;define the case for or and checking if tagged-list
 (define (or? exp) 
         (tagged-list? exp 'or)) 
 ;;this is helper function for getting the rest of the or arguments
 (define (or-cdr exp) 
         (cdr exp)) 
		 
;;;;;;;;;;;;;		 
 (define (let? exp) (tagged-list? exp 'let)) 
;;;maping the arguments
(define (let-args exp) (map cadr (cadr exp))) 
;;;mapping the variables
 (define (let-variables exp) (map car (cadr exp))) 
 ;;;mapping the variables
;;;geting the body of the expression
 (define (let-body exp) (cddr exp)) 

;;;working on the compound expression making lamba for variables, body, and args
 (define (let->combination exp) 
   (cons (make-lambda (let-variables exp) (let-body exp)) 
         (let-args exp))) 

;;force and delay and thunk
(define (delay? exp) (tagged-list? exp 'delay))
(define (thunk? obj) (tagged-list? obj 'thunk))

;;get eorrs for 'thunk out of bounds dont know what to do...
;;(define (force-it obj)
;;(if (thunk? obj)
;;(actual-value (thunk-exp obj) (thunk-env obj))
;;obj))
;;(define (actual-value exp env)
;;(force-it (mceval exp env)))


;;(define (force? obj) (tagged-list? obj 'force))
;;(define (thunk? obj) (tagged-list? obj 'thunk))
;;(define (thunk-exp thunk) (cadr thunk))
;;(define (thunk-env thunk) (caddr thunk))
;;(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
;;(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


(define (stream-cons? exp) (tagged-list? exp 'stream-cons))

		

(define (mceval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
	;;added special forms for and and or
	((and? exp) (eval-and (and-cdr exp) env)) 
  	((or? exp) (eval-or (or-cdr exp) env)) 
	;;added for let using combination
	  ((let? exp) (mceval (let->combination exp) env)) 
	;;;;added for force and cons-stream
	((delay? exp) (list 'thunk (cadr exp) env)) 
	    
        ((stream-cons? exp) (mceval (list 'cons (cadr exp) (list 'delay (caddr
exp))) env))
	
  

        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mceval (cond->if exp) env))



        ((application? exp)
         (mcapply (mceval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (mcapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))






;;;;;;

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mceval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mceval (if-predicate exp) env))
      (mceval (if-consequent exp) env)
      (mceval (if-alternative exp) env)))





(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mceval (first-exp exps) env))
        (else (mceval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mceval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mceval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))




(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables (list->mlist values)))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;chagne streamempty

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (eval-definition '(define empty-stream '()) initial-env)
    (eval-definition '(define (stream-empty? exp) (null? exp)) initial-env)
    (eval-definition '(define (stream-first exp) (car exp)) initial-env)
    (eval-definition '(define (stream-rest exp) (force (cdr exp))) initial-env)
	
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;;maceval error that uses scheme error with string thats displayed
(define (error1)
	(error "Metacircular Interpreter Aborted"))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)
	(list '>= >=)
	(list '<= <=)  
	(list 'cadr cadr) 
	(list 'cddr cddr) 
        (list 'error error1)
	(list 'force force)
	
	

        ))




(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (top-mceval exp)
  (let ((val (mceval exp (setup-environment))))
    (user-print val)))

(define the-global-environment (setup-environment))

(define input-prompt "> ")

(define (driver-loop)
  (let ((input-string (readline input-prompt)))
    (if (eof-object? input-string)
        (newline)
        (begin
          (let ((input (read (open-input-string input-string))))
            (with-handlers
                ([exn:fail? (lambda (exn)
                              (display "Error: ")
                              (display (exn-message exn))
                              (newline))])
              (add-history input-string)
              (let ((output (mceval input the-global-environment)))
                (user-print output)
                (newline)))
            (driver-loop))))))

(define (main . argv)
  (driver-loop))

(provide mceval
         setup-environment
         main)



;;problem 1
;; p2 1 hour p3 1 hour p4 1 hour p5 2 hours p6 2 hours
