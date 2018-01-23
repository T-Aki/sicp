(load "./4.3")

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        ((amb? exp) (analyze-amb exp))
        ((pernamenant-set? expr) (analyze-pernamenant-set expr)) 
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


(define (pernamenant-set? expr) (tagged-list? expr 'pernamenant-set!)) 

(define (analyze-pernamenant-set expr) 
	(let ((var (assignment-variable expr)) 
		(vproc (analyze (assignment-value expr)))) 
	(lambda (env succeed fail) 
		(vproc env 
			(lambda (val fail2) 
				(set-variable-value! var val env) 
				(succeed 'ok  fail2)) 
			fail)))) 