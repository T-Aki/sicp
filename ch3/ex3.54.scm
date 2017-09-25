;ex3.54.scm

(define factorials (cons-stream 1 factorials (stream-cdr integers)))