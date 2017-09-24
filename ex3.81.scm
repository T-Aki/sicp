;https://ja.wikipedia.org/wiki/%E7%B7%9A%E5%BD%A2%E5%90%88%E5%90%8C%E6%B3%95

(define (rand-update x)
    (remainder (remainder (+ (* 13 x) 5) 24))

(define (rand input-stream random-init)
(define random-stream
  (if (stream-null? input-stream)
      the-empty-stream
      (let ((request (stream-car input-stream)))
           (cons-stream
             (cond ((eq? request 'generate) (rand-update random-init))
                   ((number? request) (rand-update request))
                   (else (error "Unknown request --- RAND" request)))
             (rand (stream-cdr input-stream) (stream-car random-stream))))))
random-stream)