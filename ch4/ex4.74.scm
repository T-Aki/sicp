
;(a)
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) 
                               (not (empty-stream? s))) 
                             stream)))

;(b)変わらない