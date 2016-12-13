;ex1.36.scm
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;without average damping
(define (x-to-x1 n)
  (fixed-point (lambda (x) (/ (log n) (log x)))
               2.0))




;average damping
(define (x-to-x2 n)
  (fixed-point (lambda (x) (average x (/ (log n) (log x))))
               2.0))

;average damping needs lower steps



;ex1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

