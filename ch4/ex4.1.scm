(load "./4.1")

;左から右へ評価
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let (left-first (eval (first-exp exps) env))
        (cons left-first list-of-values (rest-exp exps) env))))

;右から左へ評価

(define (list-of-values exps env)
	(if (no-operands? exps)
     '()
     (let (right-first (list-of-values ( rest-exp exps) env))
       (cons (eval (first-exp exps) env)
             right-first))))


(define val 10)

(define exp '((set! val (+ val 2)) (set! val (* val 2))))

(list-of-values exp val)