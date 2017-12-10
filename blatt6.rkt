#lang racket
#|
Jakob Ambsdorf 6919840
Julius Schenke 6923104
Jonas Dreiskämper 6930232

Übungsleiter: Seppke Übungszeit: Mittwoch 10-12
|#

(require 2htdp/image)

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
Die Funktion ist eine Funktion höherer Ordnungm da eine andere Funktion als Parameter genommen wird.

|#