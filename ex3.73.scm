(define (RC r c dt)
    (define (rc i v)
        (add-streams
            (integral (scale-stream i (/ 1 C)) v dt)
            (scale-stream i r)))
    rc)

(define RC1 (RC 5 1 0.5))
