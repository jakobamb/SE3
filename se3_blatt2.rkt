#lang racket
;Jakob Ambsdorf 6919840
;Julius Schenke 6923104
;Jonas Dreiskämper 6930232

;Übungsleiter: Seppke Übungszeit: Mittwoch 10-12

(define wuff 'Flocki)
(define Hund wuff)
(define Wolf 'wuff)
(define (welcherNameGiltWo PersonA PersonB)
  (let ((PersonA 'Zaphod)
        (PersonC PersonA))
    PersonC ))

(define xs1 '(0 2 3 wuff Hund))
(define xs2 (list wuff Hund))
(define xs3 (cons Hund wuff))

;Nr. 1

;wuff
;wuff ist definiert als 'Flocki

;Hund
;Hund -> wuff -> 'Flocki

;Wolf
;Wolf definiert als 'wuff

;(quote Hund)
;gibt einfach nur 'Hund aus

;(eval Wolf)
;evaluiert zu 'Flocki, s. wuff

;(eval Hund)
;kann nicht evaluiert werden, da wuff nicht definiert ist

;(eval 'Wolf)
;evaluiert zu 'wuff

;(welcherNameGiltWo 'lily 'potter)
;gibt 'lily zurück, da let Person C als ersten Parameter der Funktion
;definiert, dieser ist in diesem Fall 'lily
;Das leigt daran, dass die auf einer Ebene stehenden Anweisungen nicht wie in einer imperativen Programmiersprache nacheinander ausgewertet werden
;Da wo PersonC als PersonA definiert wird, ist PersonA somit nur als Parameter bekannt, da die Zuweisung auf 'Zaphod noch nicht erfolgt ist

;(cdddr xs1)
;gibt '(wuff Hund) zurück da cdr die restliste nach dem ersten element zurückgibt,
;das wurde dreimal gemacht und nun ist nur noch die rest list (wuff Hund) vorhanden

;(cdr xs2)
;'(Flocki) da restliste und Hund -> wuff -> Flocki definiert ist

;(cdr xs3)
;'Flocki da nur das eine Element als 'Flocki definiert ist

;(sqrt 1/4)
;Wurzel von 1/4 ist 1/2

;(eval '(welcherNameGiltWo 'Wolf 'Hund))
;evaluiert zu 'Wolf da genau wie im oberen Beispiel

;(eval (welcherNameGiltWo 'Hund 'Wolf))
;evaluiert zu 'Flocki da Hund -> 'Flocki 

;Nr.2

;Nr. 2.1

(define (fak n)
  (if (= n 0)
      1
      (* n (fak(- n 1) ))))

;Nr. 2.2

(define (power r n)
  (if (= n 0)
      1
      (if (odd? n)
          (*(power r (sub1 n)) r)
          (sqr (power r (/ n 2))))))

;Nr. 2.3

(define (eulersche)
  (* (/ (euler 1) 2) (power 10 1001)))

(define (euler x)
  (let ([ende (/ 1(power 10 1000))]
        [zähler (/ x(fak (- x 1)))])
    (if (< zähler ende)
        0
        (+ zähler (euler (+ x 1))))))

;Nr. 3

(define (type-of typus)
  (cond [(boolean? typus) "boolean"]
        [(pair? typus) "pair"]
        [(list? typus) "list"]
        [(symbol? typus) "symbol"]
        [(number? typus) "number"]
        [(char? typus) "char"]
        [(string? typus) "string"]
        [(vector? typus) "vector"]
        [(procedure? typus) "procedure"]
        [else "unbekannt"]))

(define (id z) z)

;(type-of (* 2 3 4))
;gibt number zurück, da eine Zahl ausgerechnet wird

;(type-of (not 42))
;gibt boolean zurück, da not das Argument 42 als true interpretiert und mit der negation den boolschen wert #f erzeugt

;(type-of '(eins zwei drei))
;gibt pair zurück da eine liste auch immer ein pair ist (bzw. eine Verschachtelung von mehreren Pairs)

;(type-of '())
;gibt list zurück da es sich um eine leere list handelt

;(type-of (id sin))
;id gibt einfach den eingabewert zurück, in diesem fall sin, also -> procedure

;(type-of (string-ref "SE3" 2))
;returnt den char and der stelle 2

;(type-of (lambda (x) x))
;mithilfe von lambda wird eine funktion erstellt, deswegen -> procedure

;(type-of type-of)
;type-of ist eine procedure 

;(type-of (type-of type-of))
;gibt die ausgabe von type-of type-of zurück, diese ist immer ein string 
