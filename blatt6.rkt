#lang racket

(require racket/trace)

#|
Jakob Ambsdorf 6919840
Julius Schenke 6923104
Jonas Dreiskämper 6930232

Übungsleiter: Seppke Übungszeit: Mittwoch 10-12

1.
take:
- linear rekursiv
- direkt

drop:
- linear rekursiv
- direkt

merge:
- baumartige Rekursion
- Funktion höherer Ordnung, da eine andere Funktion als Parameter genommen wird*

merge-sort:
- Funktion höherer Ordnung, da eine andere Funktion als Parameter genommen wird*

*Laut Skript liegt eine Funktion höherer Ordnung dann vor,
wenn entweder Funktionen als Argumente erhalten oder als Wert zurückgeben.
(Definition 77)
|#

(define (take n xs [return '()])
   ;; das Kopfst \" uck einer Liste: die ersten n Elemente
   (cond
     ((null? xs) '())
     ((= n 0) return)
     (else (take (- n 1) (cdr xs) (append return (list (car xs)))))))

(trace take)