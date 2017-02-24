;ex3.1.scm

(define (make-accumulator balance)
  (lambda (amount)
    (begin (set! balance (+ balance amount))
      balance)))


(define A (make-accumulator 5))
(define B (make-accumulator 5))