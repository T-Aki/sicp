(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result (qeval (unique-query operands) (singleton-stream frame))))
       (if (and (not (stream-null? result))
                (stream-null? (stream-cdr result)))
           result
           the-empty-stream)))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)