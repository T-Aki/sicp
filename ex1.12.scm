;ex1.12
(define (pascal x y)
  (cond ((= y 1) 1)
        ((= x y) 1)
        (else (+ (pascal (- x 1) (- y 1))
                 (pascal (- x 1) y)))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (expt-iter b n 1))
