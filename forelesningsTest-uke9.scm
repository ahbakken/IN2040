;;Week 9
;(cons (+ 1 2) (+ 3 4))

(define (filter pred seq)
  (cond ((null? seq) '())
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))


;(filter
; (lambda (x) ;pred
;   (and (> x 2) (< x 8)))
; '(0 1 2 3 4 5 6 7 8 9)) ;seq


;(filter
; (lambda (x)
;        (eq? 2 x))
;  '(0 1 2 3 4 5 6 7 8 9)) ;seq

(define foo (* 2 21))
;foo

(define fee (delay (* 2 21)))
;fee ;; the result is delayed, gives us a promise
;(force fee) ;;finishes the promise made earlier

(define x '(0 1 2 3 4 5 6 7 8 9))
(define y '(10 11 12 13))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

;;don't understand 100% why this have to have define syntax to work with integers-starting-from
(define-syntax ;;explanation next week
  cons-stream
  (syntax-rules ()
    ((cons-stream head tail) (cons head (delay tail)))))

;(cons-stream 2 5)

(define (stream-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-interval (+ low 1) high))))

;(cons-stream (stream-cdr (stream-interval 1 10)) (stream-interval 1 10))

(define (stream-filter pred seq)
  (cond ((stream-null? seq) the-empty-stream)
        ((pred (stream-car seq))
         (cons-stream
          (stream-car seq)
          (stream-filter pred (stream-cdr seq))))
        (else (stream-filter pred (stream-cdr seq)))))

(define s (stream-interval 1 10))
;s ;only the first element is calculated
;(stream-cdr s) ;calculates only the 2nd element

(define (memoize proc)
  (let ((forced? #f)
        (result #f))
    (lambda ()
      (if (not forced?)
          (begin (set! result (proc))
                 (set! forced? #t)))
      result)))

(define (dengo x)
  (display "I was called!")
  x)

(define baby (delay (dengo 42)))
;baby
;(force baby)

;stream of integers
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))


(define nats (integers-starting-from 1))
;nats

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


;force the first n integers of a stream
(define (show-stream stream n)
  (cond ((= n 0) (display "...\n"))
        ((stream-null? stream) (newline))
        (else (display (stream-car stream))
              (display " ")
              (show-stream (stream-cdr stream) (- n 1)))))


(define odds (stream-filter odd? nats))
(show-stream odds 5)


















;;END
