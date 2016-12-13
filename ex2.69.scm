;ex2.69.scm

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge
        (adjoin-set (make-code-tree (car leaves)
                                    (cadr leaves))
                    (cddr leaves)))))