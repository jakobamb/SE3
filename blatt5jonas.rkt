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

(define musterung '(sterne punkte streifen))
(define farbe '(blau gruen gelb rot))
(define fuehler '(gekruemmt geschweift gerade))
(define fluegel '(elliptisch rhomibsch hexagonal))

;Rezessive gene finden mithilfe von member indem ein Merkmal vorbestimmt wird und dann das dominate gen angegeben wird
(define (rezfind merkmal dominant)
  (let ([rez (member dominant merkmal)])
    (list (list-ref rez (random (length rez))))))

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

;1.1.2

