;ex3.18.scm
;not finished
(define (loop? x)
  (let ((countered '()))
    (define (count? x)
      (cond ((null? x) #f)
            ((memq x countered) #t)
            (else (set! count? (cons x countered))
                  (countered (cdr x)))))
    (count? x)))


;3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

(loop? z)

;ERROR: invalid application: (() #0=(b c a . #0#))