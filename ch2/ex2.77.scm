;2.77.scm

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)


(define z (make-complex-from-real-imag 3 4))


(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
	 (proc (get op type-tags)))
    (if proc
	(apply proc (map contents args))
	(error
	 "No method for these types -- APPLY-GENERIC"
	 (list op type-tags)))))

(magnitude z)

(magnitude (complex rectangular 3 4))

(apply-generic magnitude (complex rectangular 3 . 4));追加のputがないとここでエラー

(apply-generic (apply-generic magnitude (rectangular 3 4)))




