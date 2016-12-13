; ex1.32.scm
#|
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
|#

;a recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) b))))

;b iterative


(define (accumulate combiner null-value term a next b)
  (define iter a result)
  (if (> a b)
      null-value
      (iter (next a) (combiner (term a) result))))

