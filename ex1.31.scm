; ex1.31.scm

;a recurcive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (term x) x)

(define (next x) (+ x 1))

 (define (factorial n)
	(product term 1 next n))

(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))

(* (product pi-term 1 next 10) 4)
;524288/160083
(exact->inexact (/ 524288 160083))
;3.2751010413348074

;b iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a))))
    (iter a 1)))


