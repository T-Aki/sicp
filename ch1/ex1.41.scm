;ex1.41.scm
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)
;21
