#lang racket

(define 1-to-4 (list 1 2 3 4))

(define (scale-list s l)
  (if (null? l)
      nil
      (cons (* (car l) s)
             (scale-list s (cdr l)))))

; For loop
(define (for-each proc list)
  (cond ((null? list) "Done")
        (else (proc (car list))
              (for-each proc
                        (cdr list))))) 
