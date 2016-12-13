;ex2.73.scm
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

#|
a
各種微分手続きを表形式に統合し、
get手続きで取り出すようにした。
理由
微分規則の特別な場合のため？

|#

;b 2.3.2参照
(define (install-sum-package)
  (define (make-sum a1 a2) (cons x y))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (deriv-sum s)
    (make-sum (deriv (addend s)) (deriv (augend s))))
  
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '(+) deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done)

(define (make-sum x y)
  ((get 'make-sum '+) x y))


(define (install-product-package)
  (define (make-product m1 m2) (cons m1 m2))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (deriv-product p)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  
  (define (tag x) (attach-tag '* x))
  (put 'deriv '(*) deriv-product)
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done)

(define (make-product x y)
  ((get 'make-product '*) x y))

(define (deriv x) (apply-generic 'deriv x))


;c
(define (install-exponent-package)
  (define (base s ) (car s))
  (define (exponet s) (cadr s))
  (define (make-exponentation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          (else (list '** b e))))
  (define (deriv-exponentation exp var)
    (let ((make-p (get 'make-product '*)))
      (make-p
        (make-p
          (exponent exp)
          (make-exponentation (base exp) (- (exponent exp) 1)))
        (deriv (base exp) var))))
  
  (define (tag x) (attach-tag '** x))
  (put 'deriv '(**) deriv-exponentation)
  (put 'make-exponentation '**
       (lambda (x y) (tag (make-exponentation x y))))
  'done)

(define (make-exponentation x y)
  ((get 'make-exponentation '**) x y))

#|
d
put の<op>と<type>を逆にする
|#




