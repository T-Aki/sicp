(load "./4.1")

(define (expand-clauses clauses) 
	(if (null? clauses) 
              'false                   
              (let ((first (car clauses)) 
                    (rest (cdr clauses))) 
              (if (cond-else-clause? first) 
                    (if (null? rest) 
                          (sequence->exp (cond-actions first)) 
                          (error "ELSE clause isn't last -- COND->IF" 
                                clauses)) 
                    (if (extended-cond? first) 
                          (make-application (make-lambda '(_cond-parameter) 
                                (make-if _cond-parameter 
                                      (make-application (extended-cond-actions first) 
                                            _cond-parameter) 
                                      (expand-clauses rest))) 
                          (cond-predicate first)) 
                          (make-if (cond-predicate first) 
                                (sequence->exp (cond-actions first)) 
                                (expand-clauses rest))))))) 

(define (extended-cond? clause) 
	(eq? (cadr (cond-actions clause)) '=>)) 

(define (make-application function parameters) 
	(cons function parameters)) 

(define (extended-cond-actions clause) 
	(caddr clause)) 