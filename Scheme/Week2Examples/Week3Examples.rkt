#lang racket

;;; Task 1: Find square root precisely by using the Newton's method.

(define (sqrt x)
  (newton (lambda y (- x (square y)))
          1))

(define (newton f quess)
  (define df (deriv f))
  (fixed-point
   (lambda x (- x (/ (f x) (df x))))
  guess))

(define deriv
  (lambda f
    (lambda x
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define dx 0.00000001)

(define (square x)
  (* x x))
  
  
;;; Task 2: Work with rational numbers.
   
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
 (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;Example
(define a (make-rat 1 2))
(define b (make-rat 1 4))
(define ans (+rat a b))
(numer ans) 
