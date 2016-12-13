(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
         (f (- n 2))
         (f (- n 3)))))
(define (ff n)
  (define (iter new old old2 count)
    (if (and (> count n) (= count n)) new
             (iter (+ new old old2) new old (+ 1 count))))
  (iter 3 2 1 3))

