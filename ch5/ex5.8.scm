; return 3
  
(define (label-exist? labels lable-name) 
  (assoc label-name labels)) 
  
(define (extract-labels text) 
  (if (null? text) 
      (cons '() '()) 
          (let ((result (extract-labels (cdr text)))) 
           (let ((insts (car result)) (labels (cdr result))) 
            (let ((next-inst (car text))) 
                 (if (symbol? next-inst) 
                     (if (label-exist? labels next-inst) 
                             (error "the label has existed EXTRACT-LABELS" next-labels) 
                             (cons insts 
                                        (cons (make-label-entry next-inst insts) labels))) 
                         (cons (cons (make-instruction next-inst) insts) 
                                     labels))))))) 