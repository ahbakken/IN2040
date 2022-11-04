(load "prekode3a.scm")

;;----------- 1 -------------

;Skriv prosedyren mem som gitt beskjeden 'memoize
;og en prosedyre returnerer en memoisert versjon av denne.

(define (mem message proc)
  ;1(a) - memorize result
  (define (memoize proc)
    (define cache (make-table)) ;cache holds past results of a proc
    (lambda args
      (or (lookup args cache) ;search for a past result
          (let ((result (apply proc args)))
            (insert! args result cache)
            result))))

    ;1(b) - forget result
    (define (unmemoize proc)
      (define cache (make-table))
      (or (lookup proc cache) 
          proc))

  (define (execute message proc)
    (cond
      ((eq? 'memoize message)
       (memoize proc))
      ((eq? 'unmemoize message)
       (unmemoize proc))))

  (execute message proc))

;(set! fib (mem 'memoize fib))
;(fib 3) ;calculates fib 0 to 3
;(fib 3) ;don't need to calculate again
;(fib 2) ;don't need to calculate
;(fib 4) ;calculates only for 4
;(set! fib (mem 'unmemoize fib))
;(fib 3)


;(set! test-proc (mem 'memoize test-proc))
;(test-proc)
;(test-proc)
;(test-proc 40 41 42 43 44)
;(test-proc 40 41 42 43 44)
;(test-proc 42 43 44)

;1(c) - explain set! define difference result

(define mem-fib (mem 'memoize fib))
;(mem-fib 3)
;(mem-fib 3)
;(mem-fib 2)

;The two first procedures uses set! while the last one uses define
;This means mem-fib is defined as the procedure while, test-proc is set to it.
;The result is that there is made a new procedure for each mem call.
;Because of this only the first number will be stored in the cache.
;Calling (mem-fib 3) two times we can see it's stored, but not (mem-fib 2)

;;----------- 2 -------------

;2(a) - converter stream and list

(define (list-to-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream
       (car lst)
       (list-to-stream(cdr lst)))))
  

(define (stream-to-list strm . n) ;with interval
  (define (strm-rec strm a)
    (if (or
         (stream-null? strm)
         (= a 0))
         '()
         (cons
          (stream-car strm)
          (strm-rec (stream-cdr strm) (- a 1))))) ;recursion
  (strm-rec strm
            (if (null? n)
                -1
                (car n))))
                
;(list-to-stream '(1 2 3 4 5))
;(stream-to-list (stream-interval 20 30))
;(show-stream nats 15)
;(stream-to-list nats 12)


;2(b) - n first elements of a stream
(define (stream-take n strm)
  (if (or
       (stream-null? strm) (= n 0))
       the-empty-stream
       (cons-stream
        (stream-car strm)
        (stream-take (- n 1) (stream-cdr strm)))))

;(define foo (stream-take 10 nats))
;foo
;(show-stream foo 5)
;(show-stream foo 20)
;(show-stream (stream-take 15 nats) 10)


;2(c) - problem with switch

;Would not work for infinite streams,
; the procedure memq would end up in an infinite loop.


;2(d) - Finish definition
(define (memqX strm) 
  (let ((elem (stream-car strm)))
    (lambda (x)
      (cond ((null? x) #f)
            ((eq? elem x) #f)
            (else #t)))))


(define (remove-duplicates strm)
  (if (stream-null? strm)
      the-empty-stream
      (cons-stream (stream-car strm)
                   (remove-duplicates
                    (stream-filter (memqX strm) strm)))))

;Stream with duplicates
;(define strm (list-to-stream '(1 4 3 3 3 4 2 11 9 8 8 8 8)))

;(show-stream (remove-duplicates strm))







;;END