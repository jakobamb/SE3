#lang racket

(require se3-bib/butterfly-module)

(show-butterfly 'red 'star 'curly 'hexagon)

#|
1.1

1.1.1
Als Datenstruktur bietet sich für jedes Merkmal eine Liste von Paaren an.
Jedes Paar besteht dabei aus dem Namen der Merkmalsausprägung sowie einer Zahl, die angibt wie Dominant das Gen ist.
Am Beispiel der Musterung bedeutet das: Sterne haben den Wert 3, Punkte 2 und Streifen 1.

Ein Vergleich der Dominanz zweier Gene besteht hiermit in dem Vergleich der zugeordneten Werte. Ist der Wert von Gen 1 höher als der von Gen 2, so ist Gen 1 dominant.
Die möglichen rezessiven Gene zu einer gegebenen Merkmalsausprägung bestehen aus den Paaren der selben Tabelle, die einen niedrigeren Wert in der zweiten Spalte aufweisen.

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
(define musterung
  (list ('sterne . 2)
        ('punkte . 

;1.2.1
(define (get-rezessive-merkmale merkmal)
  (


