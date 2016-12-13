;ex1.35.scm


(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

phi
;1.6180327868852458