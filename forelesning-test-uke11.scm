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
      

















;;END