;;exercise 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment seq)
  (let ((p (start-segment seq))
        (q (end-segment   seq)))
    (make-point (/ (+ (x-point p) (x-point q)) 2)
                (/ (+ (y-point p) (y-point q)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define seg1 (make-segment (cons 2 4) (cons 3 4)))
(define seg2 (make-segment (cons 2 1) (cons 3 1)))

;(display "Midpoint: ")
;(midpoint-segment seg1)

;;exercise 2.3

(define (perimiter seg1 seg2)
  (let ((s1 (start-segment seg1))
        (e1 (end-segment seg1))
        (s2 (start-segment seg2))
        (e2 (end-segment seg2)))
    (+ (abs (- (x-point s1) (x-point e1))) ;;length 1
       (abs (- (x-point s2) (x-point e2))) ;;length 2
       (abs (- (y-point s1) (y-point s2))) ;;height 1
       (abs (- (y-point e1) (y-point e2)))))) ;;height 2
       

(define (area seg1 seg2)
  (let ((s1 (start-segment seg1))
        (e1 (end-segment seg1))
        (s2 (start-segment seg2))
        (e2 (end-segment seg2)))
    (* (abs (- (x-point s1) (x-point e1)))  ;;length
       (abs (- (y-point e1) (y-point e2)))))) ;;height

;(display "Perimeter: ")
;(perimiter seg1 seg2)

;(display "Area: ")
;(area seg1 seg2)

;;exercise 2.24
;;(list 1 (list 2 (list 3 4)))
;;(1 (2 (3 4)))

;;exercise 2.25
;(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

;(car (car '((7))))

;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;;Exercise 2.26

;(define x (list 1 2 3))
;(define y (list 4 5 6))

;(display "append ")
;(append x y)
;'(1 2 3 4 5 6)

;(display "cons ")
;(cons x y)
;'((1 2 3) (4 5 6)) ;Wrong
;'((1 2 3) 4 5 6)   ;Right

;(display "list ")
;(list x y)
;'((1 2 3) (4 5 6))


;;Exercise 2.27

(define (my-reverse lst)
  (if (null? lst)
      lst
      (append (my-reverse (cdr lst)) (list (car lst)))))

;(my-reverse (list 1 4 9 16 25))

(define x (list (list 1 2) (list 3 4) 5 6 (list 7 8)))
;(my-reverse x)


(define (deep-reverse lst)
  (if (null? lst)
      lst
      (if (list? (car lst))
          (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst))))
          (append (deep-reverse (cdr lst)) (list (car lst))))))

;(deep-reverse x)

;;Exercise 2.28

(define (fringe lst)
  (if (null? lst)
      lst
      (if (list? (car lst))
          (append (fringe (car lst)) (fringe (cdr lst)))
          (append (list (car lst)) (fringe (cdr lst))))))

;(fringe (list x (list x x)))      

;;Exercise 2.30
(define (square x)
  (* x x))

(define (square-tree lst)
  (if (null? lst)
      lst
      (if (list? (car lst))
          (append (list (square-tree (car lst))) (square-tree (cdr lst)))
          (append (list (square (car lst))) (square-tree (cdr lst))))))

;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;;Exercise 2.31
(define (tree-map proc lst)
  (if (null? lst)
      lst
      (if (list? (car lst))
          (append (list (square-tree (car lst))) (square-tree (cdr lst)))
          (append (list (proc (car lst))) (square-tree (cdr lst))))))

(define (square-tree2 tree)
  (tree-map square tree))

(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))



;;END