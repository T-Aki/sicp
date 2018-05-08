;a

(define count-leaves-machine
  (make-machine
    (list (list '+ +)
          (list 'null? null?)
          (list 'pair? pair?)
          (list 'car car)
          (list 'cdr cdr)) 
    '((assign continue (label count-leaves-done))
      (assign val (const 0))
      pair-tree
      (test (op null?) (reg tree))
      (branch (label null-tree))
      (test (op pair?) (reg tree))
      (branch (label left-tree))
      (assign val (const 1))
      (goto (reg continue))
      left-tree
      (save tree)
      (save continue)
      (assign continue (label right-tree))
      (assign tree (op car) (reg tree))
      (goto (label pair-tree))
      right-tree
      (restore continue)
      (restore tree)
      (save continue)
      (save val)
      (assign continue (label after-tree))
      (assign tree (op cdr) (reg tree))
      (goto (label pair-tree))
      after-tree
      (assign var (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg var) (reg val))
      (goto (reg continue))
      null-tree
      (assign val (const 0))
      (goto (reg continue))
      count-leaves-done))) 

;b
(define count-leaves-machine
  (make-machine '()
                (list (list 'null? null?)
                      (list 'pair? pair?)
                      (list '+ +)
                      (list 'car car)
                      (list 'cdr cdr))
                '(controller
                  (assign n (const 0))
                  (assign continue (label iter-done))
                  iter
                  (test (op null?) (reg tree))
                  (branch (label null-tree))
                  (test (op pair?) (reg tree))
                  (branch (label pair-tree))
                  (assign n (op +) (reg n) (const 1))
                  (goto (reg continue))
                  pair-tree
                  (save continue)
                  (save tree)
                  (assign tree (op car) (reg tree))
                  (assign continue (label after-left-tree)) 
                  (goto (label iter))
                  after-left-tree
                  (restore tree)
                  (assign tree (op cdr) (reg tree))
                  (assign continue (label after-right-tree)) 
                  (goto (label iter))
                  after-right-tree
                  (restore continue)
                  (goto (reg continue))
                  null-tree
                  (goto (reg continue))
                  iter-done))) 
