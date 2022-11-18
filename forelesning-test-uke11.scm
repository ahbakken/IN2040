;forelesning-test-uke11+12
(load "evaluator.scm")


;repetition first

(define square
  (lambda (x) (* x x)))

;(square 5)

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((special-form? exp) (eval-special-form exp env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
                   (list-of-values (operands exp) env)))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands) env))))

(define (eval-special-forms exp env)
  (cond ((quoted? exp) (text-of-quatation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-dfinition exp env))
        ((if? exp) (eval-if exp env))
        ((lamba? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions esp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))))


(define (mc-apply proc args)
  (cond ((primitive-procedure? proc)
         ((apply-primitive-procedure proc args))
         ((compound-procedure? proc)
          (eval-sequence (procedure-body proc)
                         (extend-environment
                          (procedure-parameters proc)
                          args
                          (procedure-environment proc)))))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (read-evaluate-print-loop); same as driver-loop in SICP
  (prompt-for-input input-promt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (read-eval-print-loop))
      

(define (proc-if test then else)
  (if test then else))

(define (foo) (foo))

(if #f (foo) 'bar)
;(proc-if #f (foo) 'bar) ;;infinite loop

(if #f (/ 1 0) 'bar)
;(proc-if #f (/ 1 0) 'bar) ;error: divition by zer


;;thunks
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj
                'evaluated-thunk))

(define (thunk-value thunk)
  (cadr thunk))


(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value       ;; evaluate the expression
                        (thunk-exp obj)
                        (thunk-env obj))))
         (set-car! obj 'evaluated-thunk)
         (set-car! (cdr obj) result)       ;; replace expression with value
         (set-cdr! (cdr obj) '())          ;; forget the environment
         result))
  ((evaluated-thunk? obj)                  ;; memoized?
   (thunk-value obj))
  (else obj)))                             ;; not a thunk
   
(define (actual-value exp env)
  (force-it (mc-eval exp env)))


;;change in mc-eval for lazy scheme
(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((special-form? exp) (eval-special-form exp env))
        ((application? exp)
         (mc-apply (actual-value (operator exp) env) ;; change from last mc-eval
                   ((operands exp)
                    env)))))


;;change in mc-apply for lazy scheme

(define (mc-apply proc args env)
  (cond ((primitive-procedure? proc)
         ((apply-primitive-procedure
           proc (list-of-arg-values args env))) ;; Gets actaul values
         ((compound-procedure? proc)
          (eval-sequence
           (procedure-body proc)
           (extend-environment
            (procedure-parameters proc)         ;; parameters are bound
            (list-of-delayed-args args env)     ;; to thunks
            (procedure-environment proc)))))))


(define (list-of-arg-values exps env)
  (if (no-operands? exps) '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-exps exps) env))))
            
(define (list-of-delayed-args exps env)
  (if (no-operands? exps) '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))


;;streams as lazy lists
;(define (cons x y)
 ; (lambda (m)(m x y)))

;(define (car z)
 ; (z (lambda (p q) p)))

;(define (cdr z)
 ; (z (lambda (p q) q)))
;;END