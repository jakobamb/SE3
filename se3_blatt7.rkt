#lang racket

(require 2htdp/image)
(require racket/trace)

;Aufgabe 1
;allgemein rekursiv
(define (zaehlen x xs)
  (if (null? xs)
      0
      (if (equal? (car xs) x)
          (+ 1 (zaehlen x (cdr xs)))
          (zaehlen x (cdr xs)))))

;endrekursiv
(define (zaehlen_end x xs [sum 0])
  (if (null? xs)
      sum
      (if (equal? (car xs) x)
          (zaehlen_end x (cdr xs) (add1 sum))
          (zaehlen_end x (cdr xs) sum))))

;higher-order
(define (zaehlen_ho x xs)
  (foldl (lambda (a b)
           (if (equal? a x)
               (add1 b)
               b))
         0
         xs))

(trace zaehlen zaehlen_end zaehlen_ho)

(zaehlen 3 (list 2 5 7 3 56 3 6456 3 543 42))
(zaehlen_end 3 (list 2 5 7 3 56 3 6456 3 543 42))
(zaehlen_ho 3 (list 2 5 7 3 56 3 6456 3 543 42))


;Aufgabe 2

#|
2.1

Als Datenstruktur nutzen wir eine Liste von N (mit N=30) Listen, die jeweils eine Spalten des Spielfeldes repräsentiert.
Jede dieser Spalten enthält N Elemente, die durch einen Wahrheitswert anzeigen, ob ein Feld lebendig (#t) oder tot (#f) ist.

Durch die Unterteilung in Spalten kann man relativ einfach benachbarte Zellen finden, ansonsten ist die Datenstruktur auf das wesentliche reduziert,
sodass sie die in den Folgeaufgaben benötigten Operationen gut unterstützt.

TODO: Hier noch ein paar Zeilen wenn die Funktionen implementiert sind.
|#

;2.2

(define (draw-row cells xoffset yoffset scene)
  (if (null? cells)
      scene
      (place-image (square 20
                           (if (car cells)
                                  ;ausfüllen wenn lebendig, sonst nur umranden
                                  "solid"
                                  "outline")
                           "black")
                   xoffset
                   yoffset
                   (draw-row (cdr cells) (+ xoffset 20) yoffset scene))))

(define (draw-board state)
  (foldr (lambda (acc row)
           (draw-row 

(draw-row (list #t #t #f #f #f #t #f #f #f #t #t) 0 10 (empty-scene 800 600))
                           

