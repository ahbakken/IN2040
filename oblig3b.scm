;oblig 3b ahbakken
(load "evaluator.scm")

;---------------- 1 -------------------;
;;; MC-Eval input:
;(define (foo cond else)
;  (cond ((= cond 2) 0)
;        (else (else cond))))
;(define cond 3)
;(define (else x) (/ x 2))
;(define (square x) (* x x))

;;1(a)
;;; MC-Eval input:
;(foo 2 square)
;;; MC-Eval value:
;0
;; I prosedyren foo har vi 2 typer cond,
;; der posisjonen vil evalueres ulikt
;; (cond ((= cond 2) 0)
;; forste cond er har posisjon til operator eller prosedyre
;; her vil den evalueres til en special form, som tester true/false (#t/#f)
;; cond inni (= cond 2) evalueres til verdien som ble sendt inn som argument.
;; dette argumnetet er 2, som gir #t og prosedyren returnerer 0


;;; MC-Eval input:
;(foo 4 square)
;;; MC-Eval value:
;16
;; I dette uttrykket er det forste argumentet ikke 2 saa else vil slaa inn
;; her vil samme skje med else, som med cond over
;; (else (else cond))))
;; forste else er special form og skjer om cond evalueres til #f
;; andre else er argument med verdien square
;; altsaa square er en egen prosedyre som tar inn verdien til cond som er 4
;; square returnerer kvadratet av 4 (16), og foo vil returnere dette resultatet igjen,

;;; MC-Eval input:
;(cond ((= cond 2) 0)
;      (else (else 4)))
;;; MC-Eval value:
;2
;;(cond ((= cond 2) 0)
;; forste cond er i posisjon til prosedyre eller operator og evalueres som special form
;; andre cond maa hente verdien cond i fraa naermeste ramme
;; i oppgave over var cond og else definert i samme ramme
;; og man trengte ikke gaa til den globale rammen for aa hente verdier
;; Her hentes det fra den globale rammen der cond er definert til 3
;; det gir #f paa cond-testen og uttrykker ser videre til (else (else 4)))
;; forste else evalueres til special form og vil utfores om cond failer
;; her maa else hente verdien i global ramme, som er en funksjon som deler paa 2
;; 4 sendes inn til else og returnerer 2

;----------------- 2 ------------------;
;;2(a)
;; Endring gjort i prekode (evaluator.scm)

;;2(b)
(set! the-global-environment (setup-environment))
(define global the-global-environment)

(define (install-primitive! proc-name proc)
  (let ((primi-proc (list 'primitive proc)))
    (define-variable! proc-name primi-proc global)))
  

(install-primitive! 'square (lambda (x) (* x x)))
;global
;(read-eval-print-loop)
;(mc-eval '(square 3) global)

;----------------- 3 ------------------;
;;3(a)
;; Endringer gjort i her ikke i prekode
;; legger til i special-form? og eval-special-form

;; and

(define (and? exp)
  (tagged-list? exp 'and))

;; or

(define (or? exp)
  (tagged-list? exp 'or))


(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t) 
        ((let? exp) #t) ;; 3b - legger til let
        ((and? exp) #t) ;; 3a - legger til and
        ((or? exp) #t)  ;; 3a - legger til or
        (else #f)))


(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((let? exp) (eval-let exp env))       ;; 3b - legger til let
        ((and? exp) (eval-and (cdr exp) env)) ;; 3a - legger til and
        ((or? exp) (eval-or (cdr exp) env)))) ;; 3a - legger til or


(define (eval-and exp env)
  (cond ((last-exp? exp)                     ;;check for last elem/exp in the list
         (mc-eval(car exp) env))
        ((false?                             ;; as long as one exp is #f return #f
          (mc-eval (car exp) env)) #f) 
        (else (eval-and (cdr exp) env))))    ;; check the whole list for a #f


(define (eval-or exp env)
  (cond ((last-exp? exp)                     ;;check for last elem/exp in the list
         (mc-eval(car exp) env))
        ((true?                              ;; as long as one exp is #t return #t
          (mc-eval (car exp) env)) #t) 
        (else (eval-or (cdr exp) env))))     ;; check the rest of the list for a #t  


;(display "or  ")
;(mc-eval '(or 1 2 3 4 #f #f #f) the-global-environment)
;(display "and ")
;(mc-eval '(and 2 #t 1 35 #t #f) the-global-environment)


;;3(b)
(define (else? exp)
  (tagged-list? exp 'else))

(define (then? exp)
  (tagged-list? exp 'then))

(define (eval-if exp env)
  (if (then? (cddr exp))
      (eval-elsif exp env)
      (if (true? (mc-eval (if-predicate exp) env)) ;; sjekker om test er #t/#f
         (mc-eval (if-consequent exp) env)         ;; sjekker for konsekvens av test
         (mc-eval (if-alternative exp) env))))     ;; sjekker om det er else

(define (eval-elsif exp env)
   (if (not(else? exp))
       (if (true? (mc-eval (if-predicate exp) env))
           (mc-eval (if-consequent (cdr exp)) env)
           (eval-elsif (cddddr exp) env))
       (mc-eval (cadr exp) env)))


(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (if (else? (cdddr exp))
         (car (cddddr exp)) 
         (cadddr exp))
      'false))


(define exp1 '(if (eq? 2 3)
                  then 'if-test
               elsif (eq? 1 2)
                  then 'elif-test
               elsif (not #t)
                  then 'elif-fest
               elsif (+ 7 9)
                  then 'elif-best
               else '(dette er else)))

(define exp2 '(if (eq? 4 3)
                  "forste alternativ"
                  "else uten else"))

;(mc-eval exp1 the-global-environment) ;;test1
;(mc-eval exp2 the-global-environment) ;;test2

;(read-eval-print-loop)

;;3(c)
(define (let? exp)
  (tagged-list? exp 'let))

;;lagt til i special-form og eval-special-form under 3(a)


(define (eval-let exp env)
  (mc-eval (let->lambda exp env) env))

;; overforer let til lambda med map
(define (let->lambda exp env)
  (append (list (let-helper exp)) (map cadr (cadr exp))))


(define (let-helper exp)
  (list 'lambda (map car (cadr exp)) (caddr exp)))

;(mc-eval '(let ((gange *) (to 2) (fire 4)) (gange to fire)) global)

;;3(d)
;;Kommenterer ut svaret paa denne oppgaven

;(define (let->lambda exp env)
;  (append (list (let-helper exp)) (expr-pair exp)))

;(define (let-helper exp)
;  (append (list 'lambda (var-pair exp)) (cdr (member 'in exp))))

;henter expression par
;(define (expr-pair exp)
;    (cons (cadddr exp)
;          (if (equal? (car (cddddr exp)) 'and) ;; ser etter flere and
;              (expr-pair (cddddr exp))
;              '())))

;henter variabel par
;(define (var-pair exp)
;    (cons (cadr exp)
;          (if (equal? (car (cddddr exp)) 'and) ;;sjekker om det er flere 'and
;              (var-pair(cddddr exp))
;              '())))

;(read-eval-print-loop)
;(let x = 2 and y = 3 in (display (cons x y)) (+ x y))

;;END