#lang racket

;;; Task 1: Find nth Fibonacci number. 

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))


;;; Task 2: Hanoi tower movement. 

(define (move n from to spare)
  (cond ((= n 0) "Finish")
        (else
         (move (- n 1) from spare to)
         (move (- n 1) spare to from))))


;;; Task 3: Method to implement different sums easily. 

; Simple sum.
(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (+ 1 a) b))))

;; term and next are procedures.
;; Function sum below can be used to implement other sum functions.
(define (sum term a next b)
  (if ( > a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (1+ a)
  (+ 1 a))

(define (square a)
  (* a a))

(define (sum-int2 a b)
  (define (identity a) a)
  (sum identity a  1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))


;;; Task 4: Find, if number is prime?

(define (prime? number)
	(define (helper possible-divisor number)
		(cond
			((>= possible-divisor number) #t)
			((= (remainder number possible-divisor) 0) #f)
			(else (helper (+ possible-divisor 1) number))))
(helper 2 3))
