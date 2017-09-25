;ex1.38.scm
;(log (- e 2))

(= 2 (remainder x 3))

(+ (quontient x) 2)


(define (e-euler k)
  (+ 2.0 (conc-frac (lambda (i) 1.0)
                    (lambda (i)
                      (if (= 2 (remainder i 3))
                          (/ (+ i 1) 1.5)
                          1))
                    k)))
