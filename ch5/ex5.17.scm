(load "./5.2")

(define (inst-label inst) 
	(define (inst-label-iter inst lst) 
		(if (member inst (car lst)) 
			(caar lst) 
			(inst-label-iter inst (cdr lst)))) 
	(inst-label-iter inst (reverse labels)))  


((eq? message 'install-labels) 
	(lambda (lbls) (set! labels lbls) 'done)) 
((eq? message 'print-labels) 
	(lambda () labels)) 


(define (assemble controller-text machine) 
	(extract-labels controller-text 
		(lambda (insts labels) 
			((machine 'install-labels) labels)  
			(update-insts! insts labels machine) 
			insts))) 