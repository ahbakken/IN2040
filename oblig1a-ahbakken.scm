;;Obligatorisk innlevering 1a
"Oppgave 1"

;;(a)
(* (+ 4 2) 5)
;; Gir 55, ingen feil. 
;; Operatorer er til hoyre og minst to operander 


;;(b)
;;(* (+ 4 2) (5))
;; Har en feil.
;; Antar at innenfor en "()" vil det komme forst 1 operator og minst 2 operander.
;; Altsa er det mest grunnleggende templatet "(operator operand operand)"
;; Og man kan legge til sa mange operander man vil, sa lenge det er minst 2.
;; For "(5)" Interpereteren vil faa feil fordi det mangler en operand og en operator

;;(c)
;;(* (4 + 2) 5)
;; Har en feil
;; folger ikke syntaks, til venstre inni en () skal operand komme forst og deretter operatorer.
;; Her tror kompilator at 4 er operator, men det er ingen operasjon som dette symbolet er definert til a gjore.
;; Det vil derfor oppsta en feil. 

;;(d)
(define bar (/ 44 2))
bar
;; Har ingen feil.
;; Ved aa starte "()" med "define" vil interpereter anta at dette er en prosedyre.
;; Her lages en variabel som holder verdien utregnet i "(/ 44 2)"
;; Denne verdien er konstant lagret i variabelen "bar"
;; For at verdien skal printes til konsollen maa man deretter kalle paa prosedyren ved aa kun skrive navnet.

;;(e) 
(- bar 11)
;; Ingen feil
;; Folger evalueringsreglene, bar vil returnere 22 og saa trekkes fra 11

;;(f)
(/ (* bar 3 4 1) bar)
;; Ingen feil.
;; Folger evalueringsreglene som nevnt ovenfor.
;; Har operator lengst til venstre i parentes og operandene etter.
;; Tallet som bar returnerer fungerer som en operand og vil ikke forstyrre evalueringen.

"oppgave 2"
;; (a)
;; Verdiene evalueres til
(or (= 1 2)
"paff!"
"piff!"
(zero? (1 - 1)))
;; 'or' velger den forste sanne eller true (#t) verdien.
;; Kun noe som er feil eller false (#f) vil evalueres til #f
;; Resten vil evalueres til #t
;; siden (= 1 2) er #f vil "paff!" kommer paa REPL’en
(and (= 1 2)
"paff!"
"piff!"
(zero? (1 - 1)))
;; Her er resultatet #f
;; For 'and' vil resultatet blir #t eller #f.
;; Alle uttrykkene som evalueres maa vaere sanne for aa faa #t
;; Med en gang interpreter evaluerer et uttrykk som #f,
;; vil den stoppe og #f sees paa REPL'en.
(if (positive? 42)
"poff!"
(i-am-undefined))
;; Resultatet er "poff!"
;; If vil analysere to saker og starter med predikatet eller en test lengst til venstre.
;; Om predikatet er sant vil neste elemet i parentesen sendes til REPL'en
;; Om testen er #f vil neste element bli utfort.

;;(b)
;; Prosedyre som tester for negativ, positive eller 0
;; Bruker cond, sjekker saa mange kondisjonale uttrykk man vil.
;; Forste riktige man kommer til vil sendes til REPL'en,
;; derfor er det viktig aa sjekke 0 forst.
(define (sign tall)
  (cond ((= tall 0) 0)
        ((positive? tall) 1)
        ((negative? tall) -1)))
(sign -12)
;; Bruker if, som har tre deler test, utfall-hvis-sant og utfall-hvis-usant
;; Forst sjekker vi om test er #t.
;; Om den er #t vil utfall-hvis-sant utfores.
;; Om det er #f vil den hoppe til utfall-hvis-usant og utfore denne linjen.
;; Dette er en ny if-test som igjen vil ha tre deler.
(define (sign2 tall)
  (if (= tall 0)
      0
      (if (negative? tall)
          -1
          1)))
(sign2 -23)

;;(c)
;; Test uten cond eller if
(define (sign3 tall)
  (or ;; Den forste sanne vil bli sent itl REPL'en
   (and (= tall 0) 0) ;; sjekker begge om begge uttrykkene er #t og sender siste uttrykk.
   (and (< tall 0) -1) ;; tall og strenger blir evaluert til #t
   (and (> tall 0) 1))) ;;  kan bruke dette for aa sende onsket resultat til REPL'en
(sign3 -12)