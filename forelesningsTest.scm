;;"square"
(define (square x)
  (* x x))

;;"reduce"
(define (reduce proc init items)
  (if (null? items)
      init
      (proc(car items)
           (reduce proc init (cdr items)))))

;;"filter"
(define (filter pred items)
  (cond ((null? items) '())
        ((pred (car items))
         (cons (car items)
               (filter pred (cdr items))))
        (else (filter pred (cdr items)))))

;;(filter (lambda (x)
;;         (and (> x 2) (< x 8)))
;;        '(0 1 2 3 4 5 6 7 8 9))

;;"percentages"
(define (percentages items)
  (map (lambda (x) (/ x (/ (reduce + 0 items) 100)))
         items))

;;(percentages '(10 90 50 50))

;;percentage
(define (percentage items)
  (let ((sum (/ (reduce + 0 items) 100)))
       (map (lambda (x) (/ x sum))
            items)))

;;Lenght of list
(define (count-leaves tree)
  (cond ((null? tree) ;;check for empty list
         0)
        ((pair? tree) ;;check for cons-cell
         (+ (count-leaves (car tree)) ;;recursion on first element in list
            (count-leaves (cdr tree)))) ;;Recursion on rest of the list
        (else 1))) ;; add 1 when a leaf node is reached

;;(define foo-list '((1 2) 3 4 ((5 6) 7) 8))

;;(count-leaves foo-list)

(define (fringe tree)
  (cond ((null? tree) '()) ;;check for empty/end of list return, empty list
        ((pair? tree) ;; check if the node is a sub tree
         (append (fringe (car tree)) ;;appends kombine two lists, no sub lists
                 (fringe (cdr tree)))) ;;append happens in the deepest subtree
        (else (list tree))))

;;(fringe foo-list)

;;takes in tree and procedure to perform on every leaf node
(define (tree-map proc tree) 
  (cond ((null? tree) '()) ;; check for empty list
        ((pair? tree) ;;check for subtree
         (cons (tree-map proc (car tree)) ;;recursion first element in list 
               (tree-map proc (cdr tree)))) ;; recursion rest of elemens in list 
        (else (proc tree)))) ;;does the procedure on the leaf node

;;(tree-map square (fringe foo-list))


(define (tree-map1 proc tree)
  (map (lambda (subtree) 
         (if (pair? subtree)
             (tree-map1 proc subtree)
             (proc subtree)))
       tree))

;;(tree-map1 square '((1 2) 3 4))


;;tree, sets and Huffman coding - week 5
;;Lists as trees

;;count leaves in a tree

(define (count-leaves tree)
  (cond ((null? tree) 0) ;;if the tree is emtpy
        ((pair? tree) ;;if its a pair, send car and cdr of list 
         (+ (count-leaves (car tree)) ;;and add together the result sent back
            (count-leaves (cdr tree))))
        (else 1))) ;; when there is only one element send back 1 to sum together

;;(count-leaves '((1 2) 3 4))


;;check if element x is in the set
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;;put element x into a set
(define (adjoin-set x set)
  (if (element-of-set? x set) ;;check if x is in the set first
      set ;;return the same set if #t
      (cons x set))) ;;if x is not in the set put return a new set with


;;gets elemets that are in both sets and return this
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set1)) '()) ;;if one of the sets are empty, empty list returned
        ((element-of-set? (car set1) set2) ;;check if car of one set is in the other
         (cons (car set1)                  ;;if #t add this to the new set
               (intersection-set (cdr set1) set2))) ;;check the rest of the list for same element
        (else (intersection-set (cdr set1) set2))))

(define a '(1 2 3 4))
(define b '(1 4 5 6))

;;(intersection-set a b)


;;check if element x is in the set for an ordered list
(define (element-of-set? x set)
  (cond ((null? set) #f) ;;if set is empty, goes through the whole list
  ((= x (car set)) #t)  ;;when we find the set
  ((< x (car set)) #f)  ;;if x is smaller than the next element in the set, returns at this element
  (else (element-of-set? x (cdr set))))) ;;check the rest of the set

;;(element-of-set? 5 a)

;;intersection for ordered lists
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) ;;check if any of the sets are empty
      '() ;;return empty list, no intersection left in any of the lists
  (let ((x1 (car set1)) ;;x1 = first element in set1
        (x2 (car set2)));;x2 = first element in set2
    (cond ((= x1 x2) ;;if the first elements are the same
           (cons x1 (intersection-set (cdr set1) ;add this element and check the the rest of the lists
                                   (cdr set2))))
          ((< x1 x2) ;;if x1 is bigger than x2
           (intersection-set (cdr set1) set2)) ;;send all but x1, this will never appear in the set2
          ((< x2 x1) ;;if x2 is bigger than x1, 
           (intersection-set set1 (cdr set2))))))) ;;send all but x2

;;(intersection-set '(1 2 3 4) '(1 4 5 6))

;;set as a binary tree
(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree)) ;;root of the tree

(define (left-branch tree) (cadr tree)) ;;cadr gets the 2nd item in the list

(define (right-branch tree) (caddr tree)) ;;caddr gets the 3rd item in the list

(define (element-of-set? x set)
  (cond ((null? set) #f) ;; empty set/got to the end of the list
        ((= x (entry set)) #t) ;;found the element we are looking for
        ((< x (entry set)) ;;smaller element
         (element-of-set? x (left-brach set))) ;;keep looking in the left branch
        ((> x (entry set)) ;;bigger element
         (element-of-set? x (right-branch set))))) ;;keep looking in the right branch


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '())) ;;if set it empty, make tree with x
        ((= x (entry set)) set) ;;return set if x is already in the tree (the root)
        ((< x (entry set)) ;;if x is smaller than current node
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set)) ;; add x to left branch
                    (right-branch set)))
        ((> x (entry set)) ;;if x is bigger than node
         (make-tree (entry set)
                    (left-branch set) 
                    (adjoin-set x (right-branch set)))))) ;;add x to right branch


;;(adjoin-set 4 '(1 2 5 6 9))


;;Huffman trees - abstraction barrier
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf)
  (caddr x))

(define (make-code-tree left right) ;;making a node in the tree
  (list left ;;the node is a list with, left, right, symbol and sum weight
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree) ;;if the node is a leaf
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))








;;END