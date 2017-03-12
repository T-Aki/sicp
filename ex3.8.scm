;ex3.8.scm
(define init 0)

(define (f x)
  (let ((n init))
    (set! init x)
    n))

(+ (f 0) (f 1))

(define init 0)
(define (g x)
  (let ((n init))
    (set! init x)
    n))
(+ (g 1) (g 0))
