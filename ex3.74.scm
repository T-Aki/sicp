(define (sign-change-detector input-stream last-value)
    (cond
        ((and (>= input-value 0) (< last-value 0)) 1)
        ((and (< input-value 0) (>= last-value 0)) -1)
        (else 0)))

(define zero-crossings
    (stream-map sign-change-detector sense-data
        (cons-stream 0 sense-data)))

