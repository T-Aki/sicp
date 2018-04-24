(define (get-info machine info)
  ((machine 'get-info) info))

(define (gather-info controller-text)
  (define (gather inst-type insts)
    (if debug
     (format #t "\n ~a \n" insts))      
    (define (gather-iter gathered left lst)
      (if debug
       (format #t "gather-iter: \n
                  gatehered: ~a \n
                  lst: ~a \n" gathered lst))
      (cond ((null? lst) (list (cons inst-type gathered)
                              left))
            ((not (pair? (car lst)))
             (gather-iter gathered left (cdr lst)))
            ((eq? inst-type
                  (caar lst))
             (if (member (car lst)
                         gathered)
                 (gather-iter gathered left (cdr lst))
                 (gather-iter (cons (car lst) gathered)
                              left (cdr lst))))
            (else
             (gather-iter gathered (cons (car lst) left)
                          (cdr lst)))))
    (gather-iter '() '() insts))
  (define (gather-entry-points gotos)
    (define (gather-iter gathered lst)
      (if (null? lst)
          (list 'entry-points gathered)
          (let ((dest (goto-dest (car lst))))
            (if (register-exp? dest)
                (gather-iter (cons (register-exp-reg dest)
                                   gathered)
                             (cdr lst))
                (gather-iter gathered (cdr lst))))))
    (list (gather-iter '() gotos))
  (define (gather-saved-reg saved)
    (list (cons 'stacked-ref (map (lambda (x)
                                    (stack-inst-reg-name x)) saved))))
  (define (gather-sources assigns)
  (define (sources-iter reg gathered left lst)
    (cond ((null? lst)
           (list (list reg gathered) left))
          ((eq? reg (assign-reg-name (car lst)))
           (sources-iter reg
                         (cons (cddr (car lst))
                               gathered)
                         left
                         (cdr lst)))
          (else
           (sources-iter reg gathered
                         (cons (car lst)
                               left)
                         (cdr lst)))))
  (define (sources-loop insts)
    (if (null? insts)
        '()
        (let* ((reg (assign-reg-name (car insts)))
               (srcs (sources-iter reg '() '()
                                   insts)))
          (cons (car srcs)
                (sources-loop (cadr srcs))))))
  (sources-loop assigns))
  (let* ((assigns
          (gather 'assign controller-text))
         (tests
          (gather 'test (cadr assigns)))
         (branches
          (gather 'branch (cadr tests)))
         (gotos
          (gather 'goto (cadr branches)))
         (saves
          (gather 'save (cadr gotos)))
         (restores
          (gather 'restore (cadr saves)))
         (performs
          (gather 'perform (cadr restores)))
         (entry-points
          (gather-entry-points (cdar gotos)))
         (stacked
          (gather-saved-reg (cdar saves)))
         (sources
          (gather-sources (cdar assigns)))
         (registers
          (list
           (cons 'registers (map car sources)))))
    (append
     (map (lambda (x)
            (car x))
          (list assigns tests branches gotos saves
                restores performs))
     entry-points stacked sources registers)))