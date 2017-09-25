;2.56.scm

 (define (deriv expr var) 
   (cond ((number? expr) 0) 
         ((variable? expr) 
          (if (same-variable? expr var) 1 0)) 
         ((sum? expr)  
          (make-sum (deriv (addend expr) var) 
                    (deriv (augend expr) var))) 
         ((product? expr)  
          (make-sum 
            (make-product (multiplier expr) 
                          (deriv (multiplicand expr) var)) 
            (make-product (multiplicand expr) 
                          (deriv (multiplier expr) var)))) 
          ((exponentiation? expr)  
                          (make-product  
                            (make-product  
                              (exponent expr) 
                              (make-exponentiation (base expr) 
                              (make-sum (exponent expr) -1)))                                                                                                 
                            (deriv (base expr) var))) 
         (else (error "unkown expression type -- DERIV" expr)))) 
 
 
 (define (exponentiation? exp) 
   (and (pair? exp) (eq? (car exp) '**))) 
 
  (define (base exp) 
   (cadr exp)) 
   
  (define (exponent exp) 
   (caddr exp)) 
  
   (define (make-exponentiation base exp) 
   (cond ((=number? base 1) 1) 
         ((=number? exp 1) base) 
         ((=number? exp 0) 1) 
         (else  
          (list '** base exp)))) 
  