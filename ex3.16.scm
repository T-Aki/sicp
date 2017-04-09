;ex3.16.scm

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

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