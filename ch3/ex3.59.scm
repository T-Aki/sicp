;ex3.59.scm
;a.
(define (integrate-series s)
	(stream-map / s integers))

;b.
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream (integrate-series (scale-series sine-series -1))))