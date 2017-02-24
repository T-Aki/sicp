;ex.3.6.scm
(define (rand op)
  (let ((x random-init))
    (cond ((eq? op 'generate)
           (begin (set! x (rand-update x))
           x))
          ((eq? op 'reset)
           (lambda (new-value) (set! x new-value))))))

(define (random-init 1))
(define (rand-update x) (random x))