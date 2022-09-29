;;Usikker paa hvordan jeg skal lose denne
(define (cons1 x y)
  (lambda (z) (z x y)))

(define (car1 p)
  p)

(define (cdr1 p)
  p)

((cons1 1 2) cdr)