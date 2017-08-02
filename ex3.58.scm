;ex3.58.scm

(define (expand num den radix)
	(cons-stream
		(quotient (* num radix) den)
		(expand (remainder (* num radix) den) den radix)))

(expand 1 7 10)

1 4 2 8 ...
10/7

(expand 3 8 10)

3 7 5 0 0 ...

30/8