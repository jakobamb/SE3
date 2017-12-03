#lang racket

(require se3-bib/butterfly-module)
#|
Jakob Ambsdorf 6919840
Julius Schenke 6923104
Jonas Dreiskämper 6930232

Übungsleiter: Seppke Übungszeit: Mittwoch 10-12
|#

;Aufgabe 1.1
;1.1.1
;Listen für die Merkmale erstellen, welche die Dominanten Merkmale ordnen

(define musterung '(stars dots stripes))
(define farbe '(blue green yellow red))
(define fuehler '(curly curved straight))
(define fluegel '(ellipse rhomb hexagon))

;Aufgabe 1.2.1
;Rezessive gene finden mithilfe von member indem ein Merkmal vorbestimmt wird und dann das dominate gen angegeben wird
(define (rezfind merkmal dominant)
  (let ([rez (member dominant merkmal)])
    (list (list-ref rez (random (length rez))))))

(define (get-rezessive-merkmale art merkmal)
  (member merkmal art))

;Aufgabe 1.2.2
;entweder mit cond jeden typ von merkmalen benutzen oder 4 funktionen für alle merkmale einzelnt
(define (vmus dom1 dom2)
  (if (> (length (member dom1 musterung)) (length (member dom2 musterung)))
           dom1
           dom2))

(define (vfar dom1 dom2)
  (if (> (length (member dom1 farbe)) (length (member dom2 farbe)))
           dom1
           dom2))

(define (vfueh dom1 dom2)
  (if (> (length (member dom1 fuehler)) (length (member dom2 fuehler)))
           dom1
           dom2))

(define (vflue dom1 dom2)
  (if (> (length (member dom1 fluegel)) (length (member dom2 fluegel)))
           dom1
           dom2))

;1.2.3 
(define (new-schmetterling muster flfarbe füform flform)
  (list (list muster flfarbe füform flform)
        (list (list-ref musterung (random 0 3))
          (list-ref farbe (random 0 4))
          (list-ref fuehler (random 0 3))
          (list-ref fluegel (random 0 3)))))
;1.2.4 Akzessoren
(define (getfarbe liste)
  (car liste))

(define (getmuster liste)
  (cadr liste))

(define (getfueh liste)
  (caddr liste))

(define (getform liste)
  (cadddr liste))

(define (getdom schmetterling)
  (list (getfarbe schmetterling)
        (getmuster schmetterling)
        (getfueh schmetterling)
        (getform schmetterling)))

;1.2.5 Anzeigen
(define (anzeigen schmetterling)
  (print (show-butterfly (car schmetterling)
                  (cadr schmetterling)
                  (caddr schmetterling)
                  (cadddr schmetterling))))

;1.2.6
;Generieret eine Liste von möglichen Kindern eines Schmetterlingselternpaares
;als Daten vom Typ Schmetterling, wobei die Eltern
;anhand ihrer sichtbaren Merkmale erzeugt werden
(define (generiere-kinder mutter vater anzahl)
  ;prüfen, ob die argumente mutter und vater vom typ schmetterling oder eine Liste von merkmalen sind
  ;wenn vom typ schmetterling, dann ist das Objekt eine Paar von listen
  (if (list? (car mutter))
      (if (<= anzahl 0)
          '()
          (cons generiere-kind
                (generiere-kinder mutter vater (- anzahl 1))))
      ;rufe die funktion rekursiv auf, dieses mal mit schmetterlingsobjekten für mutter und vater
      (generiere-kinder (apply new-schmetterling mutter)
                        (apply new-schmetterling vater))))

;hilfsfunktion für 1.2.6, um ein einzelnes Kind zu erzeugen
(define (generiere-kind mutter vater)
  (cons