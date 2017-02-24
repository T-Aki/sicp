;ex3.2.scm

(define (make-monitored f)
  (let ((count 0))
    (define (mf message)
    (cond ((eq? message 'how-many-colls?) count)
          ((eq? message 'reset-count) (set! count 0))
          (else (begin (set! count (+ count 1))
                (f message)))))
  mf))

(define s (make-monitored sqrt))