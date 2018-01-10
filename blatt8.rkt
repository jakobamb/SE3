#lang racket

(require se3-bib/setkarten-module)
#|
Jakob Ambsdorf 6919840
Julius Schenke 6923104
Jonas Dreiskämper 6930232
Übungsleiter: Seppke Übungszeit: Mittwoch 10-12
|#


;1.1 
; Funktionen höherer Ordnung (high order functions) sind Funktionen,
; die Funktionen als Argumente erhalten oder als Wert zurückgeben.

#|
1.2
a) Da bei (a) keine Funktion übergeben oder zuückgegeben wird, handelt
es sich nicht um eine Funktion höherer Ordnung

b) Da "map" eine Funktion auf eine Liste anwendet handelt es sich um
eine Funktion höherer Ordnung (erhält Fkt + gibt Fkt zuück) -->keine Fkt zurück

c) Funktion höherer Ordnung, da Funktion map verwendet (und somit Funktion zurück gibt) --> doch ...s.o.

d)keine Funktion höherer Ordnung, da keinen Funktion übergeben/zurüchgegeben wird -->Fkt zurück --> höhere Ordnung

e)Funktion höherer Ordnung, da 


1.3 
Schweinchen gibt eine Funktion zurück, das lambda. Es fehlen aber die Werte für arg2 und arg3.
Die Werte werden dann beim Funktionsaufruf von Schweinchen an die Variablen übergeben, da es zur Umgebung gehört.

1.4
(foldl ( curry + 2) 3 '( 3 4 5 ))
evaluiert zu 21, da

3+(3+2)+(4+2)+(5+2) = 21  

(map gerade-oder-ungerade '( 4 587 74 69 969 97 459 4))
-->  '(ungerade gerade ungerade gerade gerade gerade gerade ungerade), da auf jedes Element
der Liste die Funktion "gerade-oder-ungerade" angewendet wird

(filter number? '(( a b) () 1 (()) 4 -7 "a"))
-->  '(1 4 -7),da filter jedes Element der List mit "Integer?" prüft und eine List mit den Elementen
 erstellt, bei denen true zurückgegeben wirdElemente 

((compose ( curry foldl + 0) ( curry filter (curryr < 0)))
'(5682 48 24915 -45 -6 48) )
-->  -51, da
--> filter < 0  (bei curry > 0) --> dann summe der ele.

|#

;2.1.
 (define (liste-quad xs)
              (map sqr xs))

;2.2.
;divide 9,11 rest 0, mit einem geht es einfach, aber wie mit 9 und 11?
(define (divide9 xs)
  (filter (lambda (x) (not (equal? 0 x))) (map (lambda (number)
            (if (= 0 (modulo number 9))
                number
                0)) xs)))

;irgendwie nur mit komischem Nebeneffekt
(define (divide911 xs)
  (filter (lambda (x) (not (equal? 0 x))) (map (lambda (number)
            (cond [(= 0 (modulo number 9)) number]
                  [(= 0 (modulo number 11)) number]
                 ))xs)))



;2.3.
;sum (ungerade>6)
;funktioniert noch nicht richtig
(define (oddg6 xs)
  (apply + (filter (lambda (x) (> x 6)) xs)
  (filter odd? xs)))


;2.4.
;odd?--> 2x list
;(define ())



;;3. Kartenspiel
;( define ( show-set-card n the-pattern the-mode the-color )
; n: 1 , 2 , or 3
; the-pattern:   'waves,    'oval ,  'rectangle
; the-mode:      'outline,  'solid,  'hatched
; the-color :    'red ,     'green,  'blue

;(define (is-a-set? xs)
;3.1
(define pattern '(waves oval rectangle))
(define mode '(outline solid hatched))
(define color '(red green blue))
(define count '(1 2 3))
(define attribute '(count pattern mode color))
 
(define (karte count pattern mode color)
  (list count pattern mode color))

(show-set-card 2 'waves 'outline 'red)

;--------------------------------------- test/Hilfsfunktionen
( define ( schw-mitte f arg1 )
(lambda ( arg2 arg3 ) ( f arg2 arg1 arg3 ) ) )


( define ( gerade-oder-ungerade x )
( if ( integer? x )
( if ( odd? x )
'gerade
'ungerade )
'keinIteger))



 ((compose ( curry foldl + 0 ) ( curry filter ( curryr < 0 ) ) )
'(5682 48 24915 -45 -6 48) )
