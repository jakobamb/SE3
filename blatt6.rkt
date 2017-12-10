#lang racket
#|
Jakob Ambsdorf 6919840
Julius Schenke 6923104
Jonas Dreiskämper 6930232

Übungsleiter: Seppke Übungszeit: Mittwoch 10-12
|#

(require 2htdp/image)
(require racket/trace)

#|Aufgabe 1: Formen der Rekursion
1.1 & 1.2
              lineare Rekursion   Baumrekursion   geschachtelte Rekursion   direkte Rekursion  indirekte Rekursion  Endrekursion 

take            ja                   nein               nein                     ja                      nein          nein                  
drop            ja                   nein               nein                     ja                      nein          nein           
merge           nein                 ja                 nein                     ja                      nein          nein     
merge-sort      ja                   ja                 nein                     nein                    ja            nein

take: 
Linear rekursiv, weil im Argument von dem rekursiven Aufruf nicht nochmal take benutzt wird. Direkt weil es sich direkt wieder aufruft.
Keine Baumrekursion, da sich die Funktion nur einmal selber aufruft. Keine Endrekursion da es keinen Akkumulator gibt und weil take sich nicht
alleine aufruft sondern sich mit Parameter verknüpft aufruft.

drop:
s. take

merge:
Nicht linear rekursiv, da die Funktion baumrekursiv ist. Baumrekursiv ist sie wegen wegen der zwei Fallunterscheidungen vorm rekursiven Aufruf.
Direkt, da sie sich wieder direkt selbst aufruft. Auch wieder nicht endrekursiv, s. take.

merge-sort:
Die Funktion ist baumrekursiv da sie sich selbst direkt mittels Baumrekursion wieder aufruft. Dazu ruft sie auch noch zwei andere rekursive Funktionen indirekt auf.
Die Funktion ist eine Funktion höherer Ordnungm da eine andere Funktion als Parameter genommen wird. (Definition 77)

|#

;1.1.3

(define (take n xs [return '()])
  ;; das Kopfst \" uck einer Liste: die ersten n Elemente
  (cond
    ((null? xs) '())
    ((= n 0) return)
    (else (take (- n 1) (cdr xs) (append return (list (car xs)))))))

(trace take)

;Aufgabe 2: Ihre Nikolausaufgabe

;rectangle für Baumstumpf, triangle für Zweige und star für Stern an der Spitze
;above um den Baum von oben nach unten zu bauen
(define wbaum (above/align
               "center"
               (star 40 "solid" "yellow")
               (underlay
               (triangle 80 "solid" "darkgreen")
               (triangle 100 "solid" "darkgreen")
               (triangle 130 "solid" "darkgreen"))
               (rectangle 30 50 "solid" "brown"))) 
    
;Erzeugung eines Geschenks
(define (geschenk) 
  (above/align "center"
               (beside
                ;Schleifen erstellen, horizontal spiegeln
                (rotate 135 (ellipse 15 5 "outline" "yellow"))
                (flip-horizontal
                 (rotate 135 (ellipse 15 5 "outline" "yellow"))))
               (underlay
                (underlay (rectangle 45 35 "solid" "blue")
                          (rectangle 3 35 "solid" "yellow"))
                (rectangle 45 3 "solid" "red"))))


                  
;geschenk-haufen rekursiv erzeugen (baumrekursion versucht)
(define (geschenk-haufen ebenen)
  (above/align "center"
               (geschenk)
               (if (= ebenen 1)
                   ;Abbruchbedingung mit rectangle das man nicht sieht
                   (rectangle 1 1 "solid" "red")
                   (beside/align "center"
                                 (geschenk-haufen (- ebenen 1))
                                 (geschenk-haufen (- ebenen 1))))))
;ein einzelner Stern
(define stern (overlay
               (star 20 "solid" "white")
               (star 40 "solid" "gold")))

;mehrere Sterne erstellen
(define (sterne anzahl)
(beside/align "middle"
      stern
      (if (= anzahl 1)
          ;wie bei geschenk-haufen
          (rectangle 0 0 "solid" "red")
          (beside
        (scale (random 1 4) stern)
        (sterne (sub1 anzahl))))))

;Szene kreieren
(define (create-scene)
  (above/align
   "center"
   (sterne 5)
   (beside
   (geschenk-haufen 3)
   wbaum)
   (text "Frohe Weihnachten!" 24 "red")))