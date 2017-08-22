;ex3.65.scm
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))


(define (partial-sums s)
	(cons-streams (stream-car s) 
		(add-streams (streams-cdr s) (partial-sums s))))


(define ln2-stream
  (partial-sums (ln2-summands 1)))

