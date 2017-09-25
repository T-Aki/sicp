;ex1.40.scm
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a x x) (* b x) c)))
