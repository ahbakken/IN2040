;forelesning-test-uke10
(load "prekode3a.scm")
;;precode has procedures for streams

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (memoize proc)
  (let ((forced? #f)
        (result #f))
    (lambda ()
      (if (not forced?)
          (begin (set! result (proc))
                 (set! forced? #t)))
      result)))

;(define-syntax delay
;  (syntax-rules ()
;    ((delay exp) (memoize (lambda () exp)))))

(define-syntax when
  (syntax-rules ()
    ((when condition . body)
     (if condition (begin . body)))))

;(when (> 3 1)
;  (display "I was evaluated\n")
;  42)

(define-syntax when
  (syntax-rules (do else) ;using keyworks
    ((when condition do body1 else body 2)
     (if condition body1 body2))))
;bad syntax
;(when (< 2 1)
;  do (display "this")
;  else (display "thats not ok"))


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define nats (integers-starting-from 1))

(define ones (cons-stream 1 ones))

(define (add-streams stream1 stream2)
  (stream-map + stream1 stream2))



;another way to define natural numbers
;(define nats
;  (cons-stream 1 (add-streams ones nats)))

;nats

;Will never stop checking even? in nats
;(stream-null? (stream-filter odd? (stream-filter even? nats)))


(define (stream-interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
             (stream-interleave s2 (stream-cdr s1))))) ;car of s2 is cadr of new stream


(define foo '(1 2 3 4 5 6))
(define faa '(7 8 9 10 11 12))

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

(define dengo1 (list-to-stream foo))
(define dengo2 (list-to-stream faa))
;dengo1
;dengo2

(define home (stream-interleave dengo1 dengo2))
;home

;(stream-to-list home)



(define (input-stream)
  (cons-stream (read) (input-stream)))

(define balances
  (cons-stream 100
               (add-streams balances (input-stream))))

;(show-stream balances 5)

;CMA
(define (moving-average stream)
  (define (generate s prev i)
    (let ((new (/ (+ (stream-car s) (* i prev))
                  (+ i 1.0))))
      (cons-stream new (generate (stream-cdr s) new (+ i 1)))))
  (generate stream 0 0))

;CMA of nats
(define avg-nats (moving-average nats))
(show-stream avg-nats 10)


(define avg-balance (moving-average balances))





;;END