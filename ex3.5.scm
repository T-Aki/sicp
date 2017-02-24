;ex3.5.scm
;randomが動かない?


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (* (- x2 x1)
        (- y2 y1))
     (monte-carlo trials P)))

(define (in-circle)
  (>= 1 (+ sqrt (random-in-range -1.0 1.0))
      (sqrt (random-in-range -1.0 1.0))))

(define (estimate-pi)
  (estimate-integral in-circle -1.0 1.0 -1.0 1.0 10000))

