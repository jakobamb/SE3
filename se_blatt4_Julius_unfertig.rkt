#lang racket
; Hilfsfunktion für Aufgabe 2 von Blatt 3
;; Liste aus Paaren von char -> string
;; Wir wÃ¤hlen eine Liste von Paaren, weil wir mit assoc das erste Paar finden kÃ¶nnen, dessen erstes Element
;; gleich dem SchlÃ1⁄4ssel ist. Wir kÃ¶nnen also mit Hilfe eines SchlÃ1⁄4ssel das korrespondierende Element erhalten.
;; Solche Listen von Paaren (auch Assoiziationslisten genannt) entsprechen eigentlich einer Hashmap, 
;; die wir bereits in SEII kennengelernt haben.
(define Buchstabiertafel
  '( (#\A "ALFA ")
     (#\B "BRAVO ")
     (#\C "CHARLIE ")
     (#\D "DELTA ")
     (#\E "ECHO ")
     (#\F "FOXTROTT ")
     (#\G "GOLF ")
     (#\H "HOTEL ")
     (#\I "INDIA ")
     (#\J "JULIETTE ")
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
     (#\2 "Bissotwo ")
     (#\3 "Terrathree ")
     (#\4 "Kartefour ")
     (#\5 "Pantafive ")
     (#\6 "Soxisix ")
     (#\7 "Setteseven ")
     (#\8 "Oktoeight ")
     (#\9 "Novenine ")
     (#\, "Decimal ")
     (#\. "Stop ")))

;; Hilfsfunktion für Aufgabe 2 von Blatt 3
;; Suche nach der ersten Ãœbereinstimmung mit dem ersten Element der Paare
;; Eine Funktion, die Buchstaben mittels der Buchstabiertabelle auf ihre SchlÃ1⁄4ssel abbildet
(define (char->code c)
  (cdr (assoc c Buchstabiertafel)))

;; Hilfsfunktion für Aufgabe 2 von Blatt 3
;; ASCII Rechnen... Ã¼ber den Integerumweg
;; Eine Funktion, die Kleinbuchstaben auf die entsprechenden GroÃŸbuchstaben abbildet und 
;; alle anderen Zeichen auf sich selbst.
(define (char->CoDe c)
  (char->code
   (if (char<=? #\a c #\z)
       (integer->char (- (char->integer c) 32))
       c)))

;; Hilfsfunktion für Aufgabe 2 von Blatt 3
;; Eine Funktion, die einen Text in Form eines Strings als Eingabe erhÃ¤lt und auf eine Liste der
;; BuchstabierschlÃ¼ssel als Strings abbildet

;; Aufruf der rekursiven Hilfsfunktion string->list
(define (spellout text)
  (string-append* "" (spellout-list-of-char (string->list text))))
  

;; Aufbau einer neuen Liste aus den substituierten Elementen
(define (spellout-list-of-char cl)
  (if (= (length cl) 1)
      (char->CoDe (car cl))
      (append (char->CoDe (car cl))(spellout-list-of-char (cdr cl))))
      )


#|1. Auswertung von Ausdrücken

1.1) (max(min 5(- 8 7))6)
evaluiert zu 6, da "min" den kleineren (1) und "max" den größeren der beiden Werte weitergibt
-> in dem Fall 6

1.2) `(+ ,(- 13 11)17)
evaluiert zu '(+ 2 17) ??                                                                      ;was macht `?

1.3) (cadr '(Good King Wenceslas))
evaluiert zu 'King, da cadr das erste Element der Restliste ausgibt

1.4) (cdddr '(looked out (On the feast of Steven)))
-> '(), da cdddr die Restliste der Restliste der Restliste ausgibt (was in diesem Fall die leere Liste ist)

1.5) (cons 'When '(the snow lay round about))
-> '(When the snow lay round about), da cons am Anfang einer Liste ein Element anfügt
 (und Paare deren zweites Element eine Liste ist, in der vereinfachten Listennotaion dargestellt werden) 

1.6) (cons '(Deep and) 'crisp)
-> '((Deep and) . crisp),                                                                     ;siehe non-list-pairs
 
1.7) (equal? (list 'and 'even) '(and even))
-> #t, da list eine Liste der beiden Paare stellt
(und equal lediglich Gleichheit der Elemente und nicht Identität prüft)

1.8) (eq? (list 'Rudolph 'the 'red-nosed 'reindeer)
(cons 'Rudolph '(the 'red-nosed 'reindeer)))
-> #f, da eq? Identität prüft und zwei Listen mit den selben Elementen nicht identisch sind

2. Textgenerierung

   ;(provide (all-defined-out))

   ;für Wiederverwendung von Fkt
   ;(require "Fkt-SE3_Uebung.rkt")         --> hab mir noch nicht angeschaut wie das geht




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
Peilzeichen    := - - -
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
     (spellout rufzeichen)    ; spellout hier Hilfsfunktion (bei uns heißt die glaube "string->alphabetcode" oder so)
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

(Notruf "NAUTILUS" "DEJY" "UNGEFAEHR 10sm östlich Point Nemo 48° 52’ 31,75" (string-append "RIESENKRAKE HAT DAS SCHIFF UMSCHLUNGEN" "\n"
        "GROSSES LECK IM RUPF" "\n" "20 MANN AN BORD " "\n" "TREIBEN ANTRIEBSLOS AN DER WASSEROBERFLAECHE"))

(Notruf "MALTESEFALCON" "HUQ9" "N 54°34’ 5,87’’ E8°27’ 33,41" (string-append "AUF SANDBANK AUFGELAUFEN" "\n" "UNSER SCHIFF IST 88 METER LANG" "\n"
"UND HAT EINEN SCHWARZEN RUMPF" "\n" "10 MANN AN BORD" "\n" "UNFALLZEIT 0730 UTC"))

; Aufgabe 3. Funktionen vs. Spezialformen: