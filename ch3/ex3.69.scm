;ex3.69.scm

((define (triple s t u)
    (cons-stream
        (list (stream-car s) (stream-car t) (stream-car u))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr (pairs t u)))
            (triple (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define (phythagoras-triple-numbers)
    (define (square x) (* x x))
    (define numbers (triple intergers intergers intergers))
    (stream-filter
        (lambda (x)
        (= (square (caddr x))
            (+ (square (car x)) (square (cadr x)))))
        numbers))