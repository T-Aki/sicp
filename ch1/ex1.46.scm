;ex1.46.scm
(define (close-enough? v1 v2)
  (define tolerance 0.00001)
  (< (/ (abs (- v1 v2)) v2) tolerance))

(define (iterative-improve improve close-enough?)
  (lambda (x)
    (let ((xim (improve x)))
      (if (close-enough? x xim)
          xim
          ((iterative-improve improve close-enough?) xim)))))
