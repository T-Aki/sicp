;2.78.scm
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))


(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((number? datum))
        (pair? datum) (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)))


