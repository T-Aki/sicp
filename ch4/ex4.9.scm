(load "./4.1")

(define (while? exp) (tagged-list? exp 'while)) 
(define (while-pred exp)(cadr exp)) 
(define (while-actions exp) (caddr exp)) 

(define (make-single-binding var val)(list (list var val))) 
(define (make-if-no-alt predicate consequent)(list 'if predicate consequent)) 
(define (make-combination operator operands) (cons operator operands)) 

(define (while->rec-func exp) 
	(list 'let (make-single-binding 'while-rec '(quote *unassigned*)) 
		(make-assignment 'while-rec 
			(make-lambda '() 
				(list (make-if-no-alt  
					(while-pred exp) 
					(make-begin (append (while-actions exp) 
						(list (make-combination 'while-rec '())))))))) 
		(make-combination 'while-rec '()))) 
