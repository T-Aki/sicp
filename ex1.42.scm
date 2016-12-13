;ex1.42.scm
(define (compose f g)
  (lambda (x) (f (g x))))