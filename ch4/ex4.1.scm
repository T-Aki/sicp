(load "./4.1")

;左から右へ評価
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        ))

;右から左へ評価

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        ))