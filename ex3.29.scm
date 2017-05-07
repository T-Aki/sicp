;ex3.29.scm

; (a1 or a2) == (not ((not a1) and (not a2))
(define (or-gate a1 a2 output)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (c3 (make-wire)))
    (inverter a1 c1)
    (inverter a2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)))

;遅延はand-gate*1+invertor-gate*2