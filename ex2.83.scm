;ex2.83.scm

(define (raise x) (apply-generic 'raise x))

(put 'raise 'scheme-number
     (lambda (x) (make-rational x 1)))

(put 'rase 'make-rational
     (lambda (x) (make-real (/ (numer x) (denom x)))))

(put 'raise 'real
     (lambda (x) (make-from-real-imag x 0)))