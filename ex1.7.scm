
;ex 1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y)2))

(define (good-enough? guess improved-guess)
  (< (abs (- guess improved-guess)) 0.001))

(define (improved-guess guess x) (improve guess x))

(define (sqrt x)
  (sqrt-iter 1.0 x))