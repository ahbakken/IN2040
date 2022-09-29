;;Pair and lists
;;Pull out 42 with cdr or car
;;1(f) - cdr gives (42 #t bar) and car takes the first element 42
(car(cdr '(0 42 #t bar)))
;;1(g)
(car(cdr(car '((0 42) (#t bar)))))
;;1(h)
(car(car(cdr '((0) (42 #t) (bar)))))

;;1(i) recreate 1(g)
(define bar 'bar)
;;only cons to get ((0 42) (#t bar))
(cons (cons 0 (cons 42 '())) (cons (cons #t (cons bar '())) '()))
;;only list to get ((0 42) (#t bar))
(list (list 0 42) (list #t bar))

;;Recursion over lists, higher order procedures
;;2(a) - recursion
(define (take n items)
  (if (= n 0)
      '()
      (if (null? items)
          '()
          (cons (car items)
                (take (- n 1) (cdr items))))))
(take 3 '(a b c d e f))
(take 1 '(a b c d e f))
(take 4 '(a b))
(take 4 '())

;;2(b) - tail recursion
(define (take n items)
  (define (sub-take n in out)
    (cond ((null? in) (reverse out))
          ((zero? n) (reverse out))
          (else (sub-take (- n 1) (cdr in)
                           (cons (car in) out)))))
  (sub-take n items '()))

(take 3 '(a b c d e f))
(take 1 '(a b c d e f))
(take 4 '(a b))
(take 4 '())

;;2(c) - pred test what we want
(define (take-while pred items)
  (cond ((pred (car items))
         (cons (car items)
               (take-while pred (cdr items))))
        ((null? items) '())
        (else '())))

(take-while even? '(2 34 42 75 88 103 250))
(take-while odd? '(2 34 42 75 88 103 250))
(take-while (lambda (x) (< x 100)) '(2 34 42 75 88 103 250))

;;2(d)
(define (map2 proc items1 items2)
  ;;return the empty list when we reach the end of the shortest list
  (cond ((null? items1) '()) 
        ((null? items2) '())
        ;; take first elements and use the proc to operate on them 
        (else (cons (proc (car items1) (car items2))
                    ;;send the rest of the lists recursively
                    (map2 proc (cdr items1) (cdr items2))))))

(map2 + '(1 2 3 4) '(3 4 5))

;;2(e) - get average 
(map2 (lambda (x y)
        ;;add x and y, divide on 2 to get average
       (/ (+ x y) 2))
      ;;the lists we operate on
      '(1 2 3 4) '(3 4 5))