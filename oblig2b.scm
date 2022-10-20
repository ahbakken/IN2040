;;Assignment 2b, IN2040, ahbakken

;;1a
(define (make-counter) ;;makes counter procedures, not the counter
  (let ((count 0)) ;;private variable
    (lambda () ;;procedure that counts
      (set! count (+ count 1)) ;;increase 1 every time its called
      count))) ;;returns

(define count 42)
(define c1 (make-counter)) ;;c1 set to the procedure that counts, count is set to 0
(define c2 (make-counter))

;;Test
;(c1) ;;calling the procedure, increasing local count by 1, returns count
;(c1)
;(c1)
;count ;;gets the global count variable with value 42
;(c2)

;;1b
;;Figure

;;2a
(define (make-stack stack)
  (define (push! args)
    (set! stack (append (reverse args) stack))) ;;revers incoming items and add first to stack
  (define (pop!)
    (if (null? stack)
        "Empty list"
          (set! stack (cdr stack))))
  (define (dispatch msg . args)
    (cond ((eq? msg 'push! ) (push! args))
          ((eq? msg 'stack) stack)
          ((eq? msg 'pop!) (pop!))
          (else "invalid")))
  dispatch)

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

;;Test
;(s1 'pop!)
;(s1 'stack) ;; (bar)
;(s2 'pop!) ;; popper en tom stack
;(s2 'push! 1 2 3 4)
;(s2 'stack) ;; (4 3 2 1)
;(s1 'push! 'bah)
;(s1 'push! 'zap 'zip 'baz)
;(s1 'stack) ;; (baz zip zap bah bar)

;;2b
(define (pop! stack) ;;takes a stack object
  (stack 'pop!))
(define (push! stack . args)
  (apply stack 'push! args))
(define (stack stack)
  (stack 'stack))

(pop! s1)
;(stack s1)
(push! s1 'foo 'faa)
;(stack s1)

;;3b
(define bar (list 'a 'b 'c 'd 'e))
;(list-ref bar 0) ;;list-ref gets the item at the index of the list indicated after the list
;(list-ref bar 1) ;; here the list is bamed bar
;(list-ref bar 2)
;(list-ref bar 3)
;(list-ref bar 4)



(set-cdr! (cdddr bar) (cdr bar)) ;;changes the cdr of 4th element of the list to point to the 2nd 
;(list-ref bar 0)
;(list-ref bar 1)
;(list-ref bar 2)
;(list-ref bar 3)
;(list-ref bar 4)
;(list-ref bar 5)


;;3b
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah)) ;;sets the first element in the list to point to the rest of the list
;;bah


;;3c
(define (cycle? lst)
  (define (cycle-check lst1 lst2)
    (cond
      ((null? lst1) #f) ;;empty list
      ((null? (cdr lst1)) #f) ;;just 1 element, no cycle
      ((eq? lst1 lst2) #t)
      (else (cycle-check (cdr lst2) (cddr lst1)))))
  (if (null? lst)
      #f
      (cycle-check lst (cdr lst))))
      

(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(define bah (list 'bring 'a 'towel))
 (set-car! bah (cdr bah))

;(cycle? '(hey ho)) 
;(cycle? '(la la la))
;(cycle? bah) 
;(cycle? bar)

;;3d

;;(list? bar)
;;(list? bah)
;;Fordi bar er en sirkulaer liste vil den evalueres til #f.
;;Den regnes ikke som en ekte liste,
;; fordi den ikke har et siste element som er en tom liste.

;;END