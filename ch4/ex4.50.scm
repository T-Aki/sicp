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
        ((ramb? exp) (analyze-amb exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


(define (analyze-ramb expr) 
 (analyze-amb (cons 'amb (ramb-choices expr)))) 

(define (ramb? expr) (tagged-list? expr 'ramb)) 
(define (ramb-choices expr) (shuffle-list (cdr expr)))

(define (shuffle-list list)
  (if (< (length list) 2)
      list
      (let ((item (list-ref list (random (length list)))))
        (cons item (shuffle-list (remove item list))))))
  