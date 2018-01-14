;#lang swindle

(require swindle/setf
         swindle/misc)
#|
Jakob Ambsdorf 6919840
Julius Schenke 6923104
Jonas Dreiskämper 6930232
Übungsleiter: Seppke Übungszeit: Mittwoch 10-12
|#

;Aufgabe 1

;1.1

;die Klasse Literaturbeiträge
(defclass* literaturb ()
  (schluessel :reader schl
              :initarg :schluessel)
  (autor :reader aut
         :initarg :autor)
  (jahr :reader jahr
        :initarg :jahr)
  (titel :reader tit
         :initarg :titel)
  :printer #t)

;Bücher
(defclass* buch (literaturb)
  (verlag :reader verl
          :initarg :verlag)
  (verlagsort :reader verlor
              :initarg :verlagsort)
  (reihe :reader reih
         :initarg :reihe)
  (nummer :reader num
          :initarg :nummer)
  :printer #t)

;Sammelbände
(defclass* sammelband (buch)
  (herausgeber :reader heraus
               :initarg :herausgeber)
  (seitenangabe :reader seitenan
                :initarg :seitenangabe)
  :printer #t)

;Zeitschrfitenartikel
(defclass* zeitschrift (literaturb)
  (name :reader name
        :initarg :name)
  (nummerb :reader numb
           :initarg :nummerb)
  (nummerh :reader numh
           :initarg :nummerh)
  (monat :reader mon
         :initarg :monat)
  :printer #t)

;Nessie, Beispiel für ein Buch

(define einbuch
  (make buch
        :schluessel 1
        :autor "Nessie"
        :jahr "1790"
        :titel "Mein Leben in Loch Ness: Verfolgt als Ungeheuer"
        :verlag "Minority-Verlag"
        :verlagsort "Iverness"
        :reihe "Die besondere Biographie"
        :nummer "1"
        ))

;Beispiel für einen Sammelband

(define einsammel
  (make sammelband
        :key 2
        :autor "Perfect, F."
        :jahr "1979"
        :titel "Mostly harmless - some observations concerning the third planet of the solar system"
        :verlag "Galactic Press"
        :verlagsort "Vega-System, 3rd planet"
        :reihe "Travel in Style"
        :nummer "5"
        :herausgeber "Adams, D."
        :seitenangabe "420"
        ))

;Beispiel für einen Zeitschriftenartikel

(define einezeitschrift
  (make zeitschrift
        :key 3
        :autor "Wells, H. G."
        :jahr "3200"
        :titel "Zeitmaschinen leicht gemacht"
        :name "Heimwerkerpraxis für Anfänger"
        :nummerb "550"
        :nummberh "3"
        ))

;Aufgabe 1.2

(defgeneric* cite (( li literaturb)))


(defmethod cite ((b buch))
  (string-append (aut b) " (" (jahr b) "). "(tit b) ", Band " (num b) " der Reihe: " (reih b) ". " (verl b)
                 ", " (verlor b) "."))

;Aufgabe 1.3
#|
Mithilfe von Ergänzungsmethoden kann man Methoden der Oberklassen mit Hilfsmethoden ergänzen. Bei CLOS
kann mithilfe von :before, :around und :after festgelegt werden wann diese Hilfsmethode dann ausgeführt wird.
Es wird so auch sicher gestellt, dass alle Hilfsmethoden ausgeüfhrt werden. Und es ist keine Überladung der Methoden nötig.
|#


;Aufgabe 2

;Aufgabe 2.1.1 + Aufgabe 2.2 Operationen

(defclass* speichermedien ()
  (speichertyp :reader speicht
               :initarg :speichertyp)
  (maxrspeed :reader maxs
             :initarg :maxrspeed)
  (kapazität :reader kap
             :initarg :kapazität)
  (lebensdauer :reader leb
               :initarg :lebensdauer)
  (mobilität :reader mob
             :initarg :mobilität)
  :printer #t)


(defclass* magnspei (speichermedien)
  (speichertyp :initarg "Magnetisch")
  :printer #t)

(defclass* hdd (magnspei)
  (speichertyp :initarg "HDD")
  :printer #t)

(defclass* diskette (magnspei)
  (speichertyp :initarg "Diskette")
  :printer #t)

(defclass* cd/dvd (speichermedien)
  (speichertyp :initarg "Optisch")
  :printer #t)

(defclass* halbleiter (speichermedien)
  (speichertyp :initarg "Halbleiterspeicher")
  :printer #t)

(defclass* ssd/ram (halbleiter)
  (speichertyp :initarg "SSD oder Ram")
  :printer #t)

(defclass* usb (halbleiter)
  (speichertyp :initarg "USB-Stick")
  :printer #t)


;Aufgabe 2.1.2

(defclass* magnetoop (magnspei cd/dvd)
  :printer #t)

(defclass* festsshd (ssd/ram hdd)
  :printer #t)

(defclass* bankkarte ()
  :printer #t)

;Aufgabe 2.2 generische Funktionen

(defgeneric get-speichertyp ((s speichermedien)) :combination generic-append-combination)
(defgeneric get-maxrspeed ((s speichermedien)) :combination generic-min-combination)
(defgeneric get-kapazität ((s speichermedien)) :combination generic-min-combination)
(defgeneric get-lebensdauer ((s speichermedien)) :combination generic-min-combination)
(defgeneric get-mobilität ((s speichermedien)) :combination generic-min-combination)


;Aufgabe 2.3

(defmethod get-maxrspeed ((m magnspei))
  (maxs m))
(defmethod get-maxrspeed ((mh hdd))
  (maxs mh))
(defmethod get-maxrspeed ((md diskette))
  (maxs md))
(defmethod get-maxrspeed ((c cd/dvd))
  (maxs c))
(defmethod get-maxrspeed ((h halbleiter))
  (maxs h))
(defmethod get-maxrspeed ((sr ssd/ram))
  (maxs sr))
(defmethod get-maxrspeed ((u usb))
  (maxs u))

(define usbstick (make usb :maxrspeed 15 :kapazität 64 :lebensdauer 10 :mobilität "ja"))
(define disk (make hdd :maxrspeed 35 :kapazität 1024 :lebensdauer 8 :mobilität "nein"))

(get-maxrspeed usbstick)
(get-maxrspeed disk)

#|
Es wird bei CLOS immer zuerst die Methode der Klasse mit der höchsten Präzedenz ausgeführt.
Die Präzedenzliste regelt in welcher Reihenfolge Klassen geordnet sind. Jede Klasse hat Vorrang vor
ihren Oberklassen. 
|#
