(load "./4.1")

((and? expr) (eval-and (and-clauses expr) env)) 
((or? expr) (eval-or (or-clauses expr) env)) 

(define (and? expr) (tagged-list? expr 'and)) 
(define (and-clauses expr) (cdr expr)) 
(define (eval-and exprs env) 
 (let ((v (evaln (first-exp exprs) env))) 
   (cond ((last-exp? exprs)  
    (if v v #f)) 
   (else 
     (if v 
       (eval-and (rest-exps exprs) env) 
       #f))))) 

(define (or? expr) 
 (tagged-list? expr 'or)) 
(define (or-clauses expr) 
 (cdr expr)) 
(define (eval-or exprs env) 
 (let ((v (evaln (first-exp exprs) env))) 
   (cond ((last-exp? exprs) 
    (if v v #f)) 
   (else 
     (if v 
       v 
       (eval-or (rest-exps exprs) env)))))) 


((and? expr) (evaln (and->if expr) env)) 

(define (and->if expr) 
 (expand-and-clauses (and-clauses expr))) 
(define (expand-and-clauses clauses) 
 (if (null? clauses) 
   (make-if 'true 'true 'false)         
   (let ((first (car clauses)) 
     (rest (cdr clauses))) 
   (if (null? rest)  
    (make-if first first 'false) 
    (make-if first (expand-and-clauses rest) 'false))))) 

((or? expr) (evaln (or->if expr) env))  
(define (or->if expr) 
 (expand-or-clauses (or-clauses expr))) 
(define (expand-or-clauses clauses) 
 (if (null? clauses) 
   (make-if 'true 'false 'true) 
   (let ((first (car clauses)) 
     (rest (cdr clauses))) 
   (make-if first 'true (expand-or-clauses rest))))) 