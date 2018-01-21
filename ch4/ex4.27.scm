 (load "./4.2")
(define count 0)

(define (id x)
(set! count (+ count 1)) x)

(define w (id (id 10))) 

;;; L-Eval input: 
count 
;;; L-Eval value: 
0 
Defining w doesn't evaluate it. 
;;; L-Eval input: 
w 
;;; L-Eval value: 
10 
now that w is in the prompt, w is forced to evaluate, evaluating both ids. so now w = 10, count = 2. 
;;; L-Eval input: 
count 
;;;; L-Eval value: 
2 