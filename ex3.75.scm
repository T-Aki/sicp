;
;平坦化したもので比較しないと正しく検出できない
;


(define (make-zero-crossings input-stream last-value last-avpt)
    (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
            (cons-stream (sign-change-detetor avpt last-avpt)
            (stream-car input-stream)
            avpt))))