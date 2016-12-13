(define odds '(1 3 5 7))
(define evens '(2 4 6 8))
(define primes '(2 3 5 7))

;2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))




