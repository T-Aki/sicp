; ex1.34.scm
(define (f g)
  (g 2))

(f square)


(f (lambda (z)
     (* z (+ z 1))))


(f f)

(f 2)

(2 2)
ERROR: invalid application