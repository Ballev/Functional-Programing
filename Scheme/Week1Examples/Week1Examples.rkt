; Find nth Fibonacci number.
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; Hanoi tower movement.
(define (move n from to spare)
  (cond ((= n 0) "Finish")
        (else
         (move (- n 1) from spare to)
         (move (- n 1) spare to from))))
