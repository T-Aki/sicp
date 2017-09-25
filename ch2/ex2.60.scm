;2.60

(define (element-of-set? x set)
  (cond ((null? set) #f)
      	((equal? x (car set) #t))
       (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
	(cons x set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (union-set set1 set2)
  (append set1 set2))
