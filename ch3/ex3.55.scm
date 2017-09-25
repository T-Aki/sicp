;ex3.55.scm

(define (partial-sums s)
	(cons-streams (stream-car s) 
		(add-streams (streams-cdr s) (partial-sums s))))
