ex2.85.scm


(define (project arg)
  (apply-generic 'project arg))

(put 'project 'complex
     (lambda (x) make-real (real-part x)))

;実数を有理数に投影する方法は？
;rationalizeの使用は精度に問題がある?

