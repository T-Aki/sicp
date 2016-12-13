;ex2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list))))
    (copy-to-list tree '())))

#|
a
list1_1  (1 3 5 7 9 11)
list1_2  (1 3 5 7 9 11)
list1_3  (1 3 5 7 9 11)

list2_1  (1 3 5 7 9 11)

b
list2 O(n)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
      
 append's growth of order is logn
list1 O(nlogn)


|#





