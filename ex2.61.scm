;ex2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))
        ((> x (car set))
         (cons (cat set) 
               (adjoin-set x (cdr set))))))

