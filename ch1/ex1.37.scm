;ex1.37.scm

(define (cont-frac n d k)
  (cond ((= k 0) 0)
        (else (/ (n k) 
                 (+ (d k) (cont-frac n d (- k 1)))))))
  
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
;0.6179775280898876
(/ 1 phi)
;0.6180344478216819

;iterative
(define (cont-frac2 n d k)
  (define (iter result term)
    (if (= term 0)
        result
        (iter (/ (n term)
                 (+ (d term) result))
              (- term 1))))
  (iter 0 k))