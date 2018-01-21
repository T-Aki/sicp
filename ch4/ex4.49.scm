(load "./4.3")

(define (parse-word word-list) 
	(require (not (null? *unparsed*))) 
	(set! *unparsed* (cdr *unparsed*)) 
	(list (car word-list) (amb (cdr word-list)))) 