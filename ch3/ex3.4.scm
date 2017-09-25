;ex3.4.scm

(define (make-account balance password)
  (let ((count 0))
   (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops amount)
    (error "Calling the cops, you have entered wrong password 7 times"))
  (define (dispatch pass m)
    (if (eq? pass password) 
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        (if (> count 6)
            call-the-cops
            (begin (set! count (+ count 1))
              (error "Incorrect password")))))
  dispatch))



