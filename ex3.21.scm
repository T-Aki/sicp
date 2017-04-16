;ex3.21.scm

;ポインタが二つあることに対応できてないから
(define (print-queue q) (car q))

(define q1 (make-queue))
(print-queue q1)
;()

(insert-queue! q1 'a)
;(a)
(insert-queue! q1 'b)
;(a b)
(delete-queue! q1)
;(b)
(delete-queue q1)
;()