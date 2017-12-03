#lang racket

(require se3-bib/butterfly-module)

(show-butterfly 'red 'star 'curly 'hexagon)

#|
1.1

1.1.1
Als Datenstruktur bietet sich für jedes Merkmal eine Liste an, bei der die Merkmale in ihrer Dominanz absteigend sortiert sind.

Ein Vergleich der Dominanz zweier Gene besteht hiermit in dem Vergleich der jeweiligen Position. Ist die Position von Gen 1 niedriger als der von Gen 2, so ist Gen 1 dominant.
Die möglichen rezessiven Gene zu einer gegebenen Merkmalsausprägung bestehen aus den Werten der Tabelle, die einen höhere Position aufweisen.

Damit in den Merkmalen besser gesucht und iteriert werden kann, werden diese Merkmalslisten wiederum zu einer Liste zusammengefasst.

1.1.2
Hier bietet sich ein Paar an, dass sich aus zwei Listen zusammensetzt.
Die erste Liste enthält die dominanten, die zweite Liste die rezessiven Gene.
Die Reihenfolge der Merkmale ist: Musterung, Flügelfarbe, Fühlerform, Flügelform.

1.1.3
Benötigt wird eine Funktion kinder-erzeugen, die drei Parameter nimmt: Den Vater, die Mutter (jew. als Datenstruktur wie in .2 beschrieben), sowie die Anzahl der Kinder als Ganzzahl)
Die Funktion liefert ein Kind zurück, dessen dominante und rezessive Gene von Mutter und Vater vererbt wurden.

Für die Vererbung wird eine weitere Funktion (b) benötigt, die zu einem gegebenen Merkmal zufällig das dominante oder das rezessive Gen eines Schmetterlings auswählt, das dann für die Vererbung herangezogen wird.

Für diese zwei Gene, die dann Vererbt werden, muss bestimmt werden, welches beim Kindschmetterling dominant und welches rezessiv wird.
Dazu wird eine weitere Funktion (c) benötigt, die zu zwei gegebenen Genen ein Paar zurückgibt, bei der das erste Element das dominante und das zweite Element das rezessive Gen ist.

In der Kinder erzeugen Funktion werden die Merkmale dann durchlaufen; für jedes Merkmal wird mittels (b) ein Gen der Eltern ausgewählt, diese beiden Merkmale werden mittels (c) ausgewertet.
Das dominante gen kommt dann in der Datenstruktur des Kindschmetterlings in die  erste, dominante Liste, das rezessive entsprechend in die zweite Liste.
Der so erstellte Schmetterling wird an eine Liste angehängt, die, nachdem die im Parameter angegeben Kinderzahl erreicht wurde, zurückgegeben wird.
|#

;1.2

;Merkmale
(define musterung '(stars dots stripes))
(define farbe '(blue green yellow red))
(define fuehler '(curly curved straight))
(define fluegel '(ellipse rhomb hexagon))

;1.2.1
;gibt zu einem gegebenen Merkmal und der Merkmalsart eine Liste der rezessiven Merkmale zurück.
;wertet zu #f aus, falls keine rezessiven Merkmale vorhanden sind
(define (get-rezessive-merkmale art merkmal)
  (member merkmal art))

;1.2.2
;Vergleicht zwei Mermale einer gegebenen Art auf Dominanz
;gibt die Merkmale als Paar zurück, wobei an erster Stelle das dominante Merkmal steht 
(define (vergleiche art m1 m2)
    (if (< (index-of m1 art) (index-of m2 art)))
           '(m1 . m2)
           '(m2 . m1)))

;1.2.3
;Konstruktor eines Schmetterlings anhand der gegebenen
;sichtbaren (dominanten) Merkmale und automatisches Erzeugen
;von zufälligen, rezesssiven Merkmalen.
(define (new-schmetterling muster flfarbe füform flform)
  (cons '(muster flfarbe füform flform)
        '((list-ref musterung (random 0 2))
          (list-ref farbe (random 0 3))
          (list-ref fuehler (random 0 2))
          (list-ref fluegel (random 0 2)))))







