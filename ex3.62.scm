;ex3.62.scm

(define (div-series s1 s2)
	(if (= (stream-car s2) 0)
		(error "divied by 0")
		(mul-series s1 (invert-unit-series s2)))