;;Week 8
(define (sum . args)
  (define (recurse list)
    (if (null? list)
        0
        (+ (car list)
           (recurse (cdr list)))))
  (recurse args))

;(sum 1 2 3)

(define (sum . args)
  (if (null? args)
      0
      (+ (car args)
         (apply sum (cdr args)))))
;(sum 1 2 3)


(define (sum . args)
  (apply + args))
;(sum 1 2 3)

;(cons 1 (cons 2 (cons 3 '())))
;'(1 . (2 . 3))
;'(1 . (2 . (3 . ())))
;(+ . (1 . (2 . ())))
;(sum . (1 2 3))


(define (fib n)
  (display "computing fib of ")
  (display n) (newline)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
         (+ (fib (- n 1))
            (fib (- n 2))))))

; from oblig3a
(define (mem message proc)
  ;memoize result
  (define (memoize proc)
    (define cache (make-table)) ;cache holds past results of a proc
    (lambda args
      (or (lookup args cache) ;search for a past result
          (let ((result (apply proc args)))
            (insert! args result cache)
            result))))

    ;forget result
    (define (unmemoize proc)
      (define cache (make-table))
      (or (lookup proc cache) 
          proc))           ;remove procedure, not finished


  (define (execute message proc)
    (cond
      ((eq? 'memoize message)
       (memoize proc))
      ((eq? 'unmemoize message)
       (unmemoize proc))))

  (execute message proc))

;(set! fib (mem 'memoize fib))
;(fib 3)
;(fib 3)
;(fib 4)
;(fib 2)

;(define table '((a . 1) (b . 2) (c . 3) (b1 . 44)))
;(assoc 'a table)
;(assoc 'b table)
;(assoc 'c table)
;(assoc 'b12 table)

;;associative tables
;;abstraction barrier
(define (make-table)
  (list '*table*))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))
  
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (and record (cdr record))))


;example use
(define doc (make-table))
;doc

(insert! + "sums numbers" doc)
(insert! cons "constructs pairs" doc)
(insert! append "concatenates lists" doc)
;doc


;(lookup append doc)
;(lookup map doc) 


;two dimensional table
(define twodim (make-table))

(define mathdim (make-table))

(insert! + 42 mathdim)
(insert! - 45 mathdim)
(insert! * 43 mathdim)

(define letters (make-table))

(insert! 'a 97 letters)
(insert! 'b 48 letters)
(insert! 'c 36 letters)

;adds lists to the list (twodim)
(insert! "math" mathdim twodim) 
(insert! "letters" letters twodim)
;twodim

;binding, name and procedures
(define foo
  (lambda args ;;variable amount of parameters (args can be 1 or more param...)
    (display "args = ")
    (display args) ;printed on same line as above
    (newline)
    (if (not (null? args))
        (apply foo (cdr args)))))


;(foo 1 2 3)
(define bar foo) ;; aliasing
;(bar 1 2 3)

;(set! foo (lambda args (display "new foo called! ")))
;(foo 1 2)
;(newline)
;(bar 1 3)
(newline)

(set! foo (let ((old-foo foo))      ;; let-over-lambda
            (lambda args (display "new new foo called! ")
              (apply old-foo args))))



;(foo 1 2)

;(bar 1 3)


;memoization part II

(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)

(set! fib (mem 'unmemoize fib)) 
(fib 3)








;;END