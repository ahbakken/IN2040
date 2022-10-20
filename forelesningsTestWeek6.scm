;;week 6
;;local state, destructive operations, omgivelsesmodellen (?)
;;change variable with set!

(define balance 100)
;(display "Balance before: ")
;balance

(set! balance (- balance 20))
;(display "Balance after: ")
;balance


(define balance 100)

(define withdraw
  (lambda (amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds")))

;(withdraw 20)
;(withdraw 200)

;;redefine withdraw
;; change to cond instead of if+begin
(define withdraw
  (lambda (amount)
    (cond ((>= balance amount)
           (set! balance (- balance amount))
          balance)
        (else "Insufficient funds"))))

;(withdraw 20)
;(withdraw 20)

;;redefine with encapsulation 
(define withdraw
  (let ((balance 100))
  (lambda (amount)
    (cond ((>= balance amount)
           (set! balance (- balance amount))
          balance)
        (else "Insufficient funds")))))

;(withdraw 20)
;(withdraw 20)

;;counter procedure
(define counter
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(counter)
(counter)
(counter)




(define make-withdraw
  (lambda (balance)
    (lambda (amount)
      (cond ((>= balance amount)
             (set! balance (- balance amount))
             balance)
            (else "Insufficient funds")))))
            

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 200))

(w1 50)
(w2 133)

(define (make-account balance)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (dispatch message)
    (cond ((eq? message 'deposit) deposit)
          ((eq? message 'withdraw) withdraw)
          ((eq? message 'balance) balance)))
  dispatch)



(define a1 (make-account 1000))
((a1 'withdraw) 100)
(a1 'balance)
















;;END