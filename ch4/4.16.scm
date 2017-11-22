(load "./4.1")
;a
(define (lookup-variable-value var env) 
	(define (env-loop env) 
		(define (scan vars vals) 
			(cond ((null? vars) 
				(env-loop (enclosing-enviroment env))) 
			((eq? var (car vars)) (car vals)) 
			(else (scan (cdr vars) (cdr vals))))) 
		(if (eq? env the-empty-environment) 
			(error "Unbound variable" var) 
			(let ((frame (first-frame env))) 
				(scan (frame-variables frame) 
					(frame-values frame))))) 
	(let ((value (env-loop env))) 
		(if (eq? value '*unassigned*) 
			(error "Unassigned varable: *unassigned*") 
			value)))
 
(define lookup-variable-value lookup-variable-value)

;b
(define (split-body-out-defines body) 
	(if (null? body) 
		(let ((defines '()) 
			(others '())) 
		(cons defines others)) 
		(let ((exp (car body)) 
			(rest (split-body-out-defines (cdr body)))) 
		(if (definition? exp) 
			(cons (cons exp (car rest)) (cdr rest)) 
			(cons (car rest) (cons exp (cdr rest)))))))

(define (make-let varvals body) 
	(list 'let varvals body)) 

(define (defines->let-defines-body defines) 
	(if (null? defines) 
		(let ((let-defines '()) 
			(let-body '())) 
		(cons let-defines let-body)) 
		(let* ((rest-let-defines-body (defines->let-defines-body (cdr defines))) 
			(rest-defines (car rest-let-defines-body))(rest-body (cdr rest-let-defines-body)) 
			(name  (definition-variable (car defines))) 
			(value (definition-value    (car defines))) 
			(current-define (list name ''*unassigned*)) 
			(current-body   (list 'set! name value))) 
		(cons (cons current-define rest-defines) 
			(cons current-body   rest-body))))) 

(define (scan-out-defines procedure-body) 
	(let* ((splited-body (split-body-out-defines procedure-body)) 
		(defines (car splited-body)) 
		(others  (cdr splited-body)) 
		(let-defines-body (defines->let-defines-body defines))) 
	(list (append (list 'let 
		(car let-defines-body)) 
	(append (cdr let-defines-body) 
		others)))))

;c

(define (contain-defines exps) 
	(if (null? exps) 
		false 
		(or (if (definition? (car exps)) 
			true 
			false) 
		(contain-defines (cdr exps))))) 

(define (make-procedure parameters body env) 
	(if (contain-defines body) 
		(list 'procedure parameters (scan-out-defines body) env) 
		(list 'procedure parameters body env))) 

(define make-procedure make-procedure) 
  