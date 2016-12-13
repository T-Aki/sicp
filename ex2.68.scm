;ex2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond 
    ((leaf? tree) '())
    ((member symbol (symbols tree))
     (let ((left (left-branch tree)) 
           (right (right-branch tree)))
       (if (member (symbol (symbols left)))
           (cons 0 (encode-symbol symbol left))
           (cons 1 (encode-symbol symbol right)))))
    (else (error "bad symbol"))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;? error