#lang racket
(require racket/trace)

;(provide (all-defined-out))
; für Wiederverwendung von Fkt                 
;(require "se3_blatt4.rkt")

#|
Blatt 4
Julius Schenke (6923104), Jakob Ambsdorf (6919840), Jonas Dreiskämper(6930232)
Mittwoch 10-12 Uhr (Benjamin Seppke)
|#
   
;---------------------------------------------------------------------------------------------
#|
(Hilfsfunktionen vom 3. Blatt)
  Wir sollten ja eigentlich die Funktionen vom letzten Aufgabenblatt verwenden aber
  das hat bei uns noch nicht funktioniert und deswegen hab ich erstmal die Hilfsfunktionen hier reinkopiert.

1.1

Liste aus Paaren von char -> string
Wir wählen eine Liste von Paaren, weil wir mit assoc das erste Paar finden können, dessen erstes Element
gleich dem Schlü1ssel ist. Wir können also mit Hilfe eines Schlüssel das korrespondierende Element erhalten.
|#
(define Buchstabiertafel
  '( (#\A "ALPHA ")          
     (#\B "BRAVO ")
     (#\C "CHARLIE ")
     (#\D "DELTA ")
     (#\E "ECHO ")
     (#\F "FOXTROTT ")
     (#\G "GOLF ")
     (#\H "HOTEL ")
     (#\I "INDIA ")
     (#\J "JULIET ")
     (#\K "KILO ")
     (#\L "LIMA ")
     (#\M "MIKE ")
     (#\N "NOVEMBER ")
     (#\O "OSCAR ")
     (#\P "PAPA ")
     (#\Q "QUEBEC ")
     (#\R "ROMEO ")
     (#\S "SIERRA ")
     (#\T "TANGO ")
     (#\U "UNIFORM ")
     (#\V "VIKTOR ")
     (#\W "WHISKEY ")
     (#\X "X-RAY ")
     (#\Y "YANKEE ")
     (#\Z "ZULU ")
     (#\0 "Nadazero ")
     (#\1 "Unaone ")
     (#\2 "Duotwo ")
     (#\3 "Terrathree ")
     (#\4 "Carrefour ")
     (#\5 "Pentafive ")
     (#\6 "Soxisix ")
     (#\7 "Setteseven ")
     (#\8 "Oktoeight ")
     (#\9 "Novonine ")
     (#\, "Decimal ")
     (#\. "Stop ")))

;; Suche nach der ersten Übereinstimmung mit dem ersten Element der Paare
;; Eine Funktion, die Buchstaben mittels der Buchstabiertabelle auf ihre Schlüssel abbildet
(define (char->code c)
  (cdr (assoc c Buchstabiertafel)))

;; Eine Funktion, die Kleinbuchstaben auf die entsprechenden Großbuchstaben abbildet und 
;; alle anderen Zeichen auf sich selbst.
(define (char->CoDe c)
  (char->code
   (if (char<=? #\a c #\z)
       (integer->char (- (char->integer c) 32))
       c)))


; Eine Funktion, die einen Text in Form eines Strings als Eingabe erhält und auf eine Liste
;der Buchstabierschlüssel als Strings abbildet

;; Aufruf der rekursiven Hilfsfunktion string->list
(define (spellout text)
  (string-append* "" (spellout-list-of-char (string->list text))))
  

;; Aufbau einer neuen Liste aus den substituierten Elementen
(define (spellout-list-of-char cl)
  
  (if (= (length cl) 1)
      (char->CoDe (car cl))
      (append (char->CoDe (car cl))(spellout-list-of-char (cdr cl)))))

;---------------------------------------------------------------------------------------------

#|1. Auswertung von Ausdrücken

1.1) (max(min 5(- 8 7))6)
evaluiert zu 6, da "min" den kleineren (1) und "max" den größeren der beiden Werte weitergibt
-> in dem Fall 6

1.2) `(+ ,(- 13 11)17)
evaluiert zu '(+ 2 17),da ??                                                                      ;TODO: was macht `?

1.3) (cadr '(Good King Wenceslas))
evaluiert zu 'King, da cadr das erste Element der Restliste ausgibt

1.4) (cdddr '(looked out (On the feast of Steven)))
-> '(), da cdddr die Restliste der Restliste der Restliste ausgibt (was in diesem Fall die leere Liste ist)

1.5) (cons 'When '(the snow lay round about))
-> '(When the snow lay round about), da cons am Anfang einer Liste ein Element anfügt
 (und Paare deren zweites Element eine Liste ist, in der vereinfachten Listennotaion dargestellt werden) 

1.6) (cons '(Deep and) 'crisp)
-> '((Deep and) . crisp)
-> cons fügt wieder 2 Elemente zu einem Paar zusammen (in dem Fall ist das erste Element eine Liste)
 
1.7) (equal? (list 'and 'even) '(and even))
-> #t, da list eine Liste der beiden Paare stellt
(und equal lediglich Gleichheit der Elemente und nicht Identität prüft)

1.8) (eq? (list 'Rudolph 'the 'red-nosed 'reindeer)
(cons 'Rudolph '(the 'red-nosed 'reindeer)))
-> #f, da eq? Identität prüft und zwei Listen mit den selben Elementen nicht identisch sind





2. Textgenerierung

Aufgabe 2.1 Grammatik Backus-Naur Form

Notmeldung     := <Übersschrift> 
                  <Standortangabe> 
                  <Notfallart> 
                  <Hilfeleistung> 
                  <Peilzeichen>
                  <Unterschrift>
                  <Ende>

Überschrift    := MAYDAY MAYDAY MAYDAY
                  HIER IST
                  <Schiffsname> <Schiffsname> <Schiffsname>
                  DELTA ECHO
                  <Rufzeichen>
                  MAYDAY
                  <Schiffsname>
                  ICH BUCHSTABIERE
                  <Schiffsnamebuchstabiert>
                  RUFZEICHEN
                  <Rufzeichenbuchstabiert>


Schiffsname    := text
Rufzeichen     := text
Schiffsnamebuchstabiert := text
Rufzeichenbuchstabiert  := text
Notfallart     := text
Hilfeleistung  := text
Peilzeichen    := - - 
Unterschrift   := <Schiffsname> <Rufzeichenbuchstabiert>
Ende           := OVER
|#

(define md "MAYDAY ")
(define ueberschrift (string-append md md md))
(define ichBin "HIER IST ")
(define buchstabiere "ICH BUCHSTABIERE ")
(define rz "RUFZEICHEN ")
(define np "NOTFALLPOSITION ")
(define ende "OVER")

; Aufgabe 2.2 Der Generator
(define (Notruf schiffsname rufzeichen position notfallart)
  
  (if (and (string? schiffsname)
           (string? rufzeichen)
           (string? position)
           (string? notfallart))
      
   (display 
    (string-append 
     ueberschrift
     "\n"
     ichBin
     "\n"
     schiffsname " "
     schiffsname " "
     schiffsname " "
     (spellout rufzeichen) 
     "\n"
     md
     schiffsname " "
     buchstabiere
     (spellout schiffsname)
     "\n"
     rz
     (spellout rufzeichen)
     "\n"
     np
     position
     "\n"
     notfallart
     "\n"
     schiffsname " "
     (spellout rufzeichen)
     "\n"
     ende
     "\n"
     "\n"))
   
(display "Falsche Eingabe Strings erwartet!")))


; Aufgabe 2.3 Der Test

(Notruf "UNICORN" "CRN" "UNGEFÄHR 5 SM NORDWESTLICH LEUCHTTURM ROTER SAND" (string-append "NOTFALLZEIT 1000 UTC" "\n" 
 "SCHWERE SCHLAGSEITE WIR SINKEN"  "\n" "KEINE VERLETZTEN" "\n"  "SECHS MANN GEHEN IN DIE RETTUNGSINSEL"  "\n" "SCHNELLE HILFE ERFORDERLICH"))

(Notruf "NAUTILUS" "DEJY" "UNGEFAEHR 10sm östlich Point Nemo 48° 52’ 31,75" (string-append "RIESENKRAKE HAT DAS SCHIFF UMSCHLUNGEN" "\n"
        "GROSSES LECK IM RUPF" "\n" "20 MANN AN BORD " "\n" "TREIBEN ANTRIEBSLOS AN DER WASSEROBERFLAECHE"))

(Notruf "MALTESEFALCON" "HUQ9" "N 54°34’ 5,87’’ E8°27’ 33,41" (string-append "AUF SANDBANK AUFGELAUFEN" "\n" "UNSER SCHIFF IST 88 METER LANG" "\n"
"UND HAT EINEN SCHWARZEN RUMPF" "\n" "10 MANN AN BORD" "\n" "UNFALLZEIT 0730 UTC"))

;; Aufgabe 3. Funktionen vs. Spezialformen:
; innere vs. äußere Reduktion

( define (hoch3 x ) ( * x x x ) )
(hoch3 (+ 3 (hoch3 3 ) ) ) 

;Innere Reduktion
;(hoch3 (+ 3( * 3 3 3))))
;(hoch3 ( + 3 (27)))
;(hoch3 ( + 3 27 ))
;(hoch3 30)
;(* 30 30 30)
;> (27000)

;;Äußere Reduktion
;(hoch3 (+ 3(hoch3 3)))
;(* (+ 3 (hoch3 3))) (+ 3 (hoch3 3)))(+ 3 (hoch3 3))))
;(* (+ 3 ( * 3 3 3 ))) (+ 3 (hoch3 3)))(+ 3 (hoch3 3))))
;(* (+ 3 27) (+ 3 ( hoch3 3))) (+ 3 (hoch3 3))))
;(* 30 (+ 3 (hoch3 3))) (+ 3 (hoch3 3))))
;(* 30 (+ 3 (hoch3 3))) (+ 3 (hoch3 3))))
;(* 30 (+ 3 27)) (+ 3 (hoch3 3))))
;(* 30 30 (+ 3 (hoch3 3))))
;(* 30 30 (+ 3(* 3 3 3))))
;(* 30 30 ( + 3 27))
;(* 30 30 30)
;> (27000)



;;3.2 Reduktionsstrategien
; Bei Racket werden beide verwendet
; Funktionen -> innere Reduktion
; Spezialformen -> äußere Reduktion


;;3.3 new-if


(define (new-if condition? then-clause else-clause)
   (cond (condition? then-clause)
         (else else-clause )))

(define (faculty product counter max−count)
   ( new-if (> counter max−count)
            product
            (faculty (* counter product)
                      (+ counter 1)
                        max−count)))



;Das Programm läuft in eine Endlosschleife.
;Die äußere Reduktion wird hier benötig, weil zuerst die If Funktion aufgelöst werden muss (bevor die darin enthaltenen Blöcke ausgeführt werden).
;In dem Beispiel wird versucht zuerst den Else-Zweig zu evaluieren.
; -> führt zu Endlosschleife, da nie die Abbruchbedingung ausgeführt wird.




