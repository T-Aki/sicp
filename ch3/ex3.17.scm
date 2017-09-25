;ex3.17.scm
(define (count-pairs x) 
  (let ((countered '())) 
    (define (helper x) 
      (if (or (not (pair? x)) (memq x countered)) 
          0
          (begin
            (set! countered (cons x countered)) 
            (+ (helper (car x))
               (helper (cdr x))
               1)))) 
    (helper x))) 


(define pairs1 '(a b c))
(count-pairs pairs1)


(define x '(a))
(define y (cons x x))
(define pairs2 (list y))
(count-pairs pairs2)


(define x '(a))
(define y (cons x x))
(define pairs3 (cons y y))
(count-pairs pairs3)

