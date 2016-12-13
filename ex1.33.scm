; ex1.33.scm

(define (filterd-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filterd-accumulate filter combiner null-value term (next a) next b))
          (combiner null-value
                    (filterd-accumulate filter combiner null-value term (next a) next b)))))


;a
(define (sum-of-square-of-primes a b)
  (filterd-accumulate prime? + 0 square a inc b))

;b
(define (gcd-filter i n)
  (if (and (= 1 (gcd i n)) (< i n))
      #t
      #f))
(define (term x) x)

(define (product-of-gcd-prime a b)
  (filterd-accumulate gcd-filter * 1 term a inc b))


