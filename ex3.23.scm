;ex3.23.scm

(define (make-deque) (cons '() '())) 

(define (front-ptr deque) (car deque)) 

(define (rear-ptr deque) (cdr deque)) 

;Θ(1)?