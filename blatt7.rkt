#lang racket

(require 2htdp/image)
(require racket/trace)
(require 2htdp/universe)

#|
Jakob Ambsdorf 6919840
Julius Schenke 6923104
Jonas Dreiskämper 6930232

Übungsleiter: Seppke Übungszeit: Mittwoch 10-12
|#

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

|#

;2.2
(define (draw-row cells xoffset yoffset scene)
  (if (null? cells)
      scene
      (place-image (square 10
                           (if (car cells)
                               ;ausfüllen wenn lebendig, sonst nur umranden
                               "solid"
                               "outline")
                           "black")
                   xoffset
                   yoffset
                   (draw-row (cdr cells) (+ xoffset 10) yoffset scene))))

(define (draw-board state [yoffset 10] [scene (empty-scene 600 600)])
  (if (null? state)
      scene
      (draw-row (car state)
                10
                yoffset
                (draw-board (cdr state) (+ yoffset 10)))))

(draw-board (list (list #f #f #t #t #f #f #f #f #t #t #f #t #f #t)
                  (list #f #t #f #t #f #f #f #f #t #t #f #t #t #f)
                  (list #f #t #f #t #f #f #f #f #t #t #f #t #t #f)

                  (list #f #f #t #t #f #f #f #f #f #f #t #f #f #t)))

;2.3

;Funktion, die für bel. Index des Spielfeldes mithilfe list-ref die Werte der 8er-Nachbarschaft ermittelt

(define (getzelle liste y x)
  (if (and (<= 0 y 29) (<= 0 x 29))
      (list-ref (list-ref liste y) x)
      #f))

(define (nachbarn liste y x)
  (list
   (getzelle liste y (- x 1))
   (getzelle liste y (+ x 1))
   (getzelle liste (+ y 1) (+ x 1))
   (getzelle liste (+ y 1) (- x 1))
   (getzelle liste (- y 1) (- x 1))
   (getzelle liste (+ y 1)  x)
   (getzelle liste (- y 1) x)
   (getzelle liste (- y 1) (+ x 1))))

;berechnet den nächsten zustand für eine einzelne Zelle
(define (next-cellstate nachbarn alive)
  (let ([anzahl-nachbarn (foldl (lambda (elem acc)
                                 (if elem
                                     (add1 acc)
                                     acc)) 0 nachbarn)])
    (if (or (and alive (equal? anzahl-nachbarn 2))
            (equal? anzahl-nachbarn 3))
        #t
        #f)))

;berechnet den nächsten zustand für das gesamte spielfeld
(define (next-state state)
  (map (lambda (row y)
         (map (lambda (cell x)
                (next-cellstate (nachbarn state y x) cell))
              row
              (range 30)))
       state
       (range 30)))

;Aufgabe 2.4

(define (game-of-life tick)
  (big-bang start
    (on-tick next-state tick)
    (to-draw  draw-board)))


(define start
  (list
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #t #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #t #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
      (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))

(game-of-life 0.25)

                  