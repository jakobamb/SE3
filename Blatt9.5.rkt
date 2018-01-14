;#lang racket
#lang swindle

(require swindle/setf
         swindle/misc)

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
        :schluessel 2
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
        :schluessel 3
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

;Aufgabe 2.1 

(defclass* speichermedium ()
(name :accessor na
  :iniarg :name)

(geschwindigkeit :accessor mg
:initarg :geschwindigkeit)
  ;...
  :reader #t
  )


(defclass* speichermedium-fest (speichermedium)
  ;attribut entfernbar #f ...
  )
(defclass* speichermedium-herausnehmbar (speichermedium) )

(defclass* opt-speichermedium (speichermedium)
;speicherart :initarg :speicherart :accessor sp-a
  )
(defclass* magn-speichermedium (speichermedium) )
(defclass* halbl-speichermedium (speichermedium))


(defclass* hdd (speichermedium-fest magn-speichermedium))
(defclass* disk (speichermedium-herausnehmbar magn-speichermedium))
(defclass* cd/dvd (speichermedium-herausnehmbar opt-speichermedium))
(defclass* ssd (speichermedium-fest halbl-speichermedium))
(defclass* ram (speichermedium-fest halbl-speichermedium))
(defclass* usb (speichermedium-herausnehmbar halbl-speichermedium))
(defclass* mag-opt-disc (speichermedium-herausnehmbar opt-speichermedium magn-speichermedium))
(defclass* bankkarte (speichermedium-herausnehmbar magn-speichermedium) )
(defclass* sshd (hdd ssd))

;------ 2.2

( defgeneric Speichertyp ((speichermedium))
:combination generic-append-combination)

( defgeneric geschwindigkeit ((speichermedium))
:combination generic-min-combination)

( defgeneric speicherplatz ((speichermedium))
:combination generic-+-combination)

( defgeneric lebensdauer ((speichermedium))
   :combination generic-min-combination)        

( defgeneric mobilitaet ((speichermedium))
:combination generic-append-combination)

#|

( defmethod Speichertyp ((speichermedium))
...)
( defmethod geschwindigkeit ((speichermedium))
...)
( defmethod speicherplatz ((speichermedium)))
( defmethod lebensdauer ((speichermedium)))
( defmethod mobilitaet ((speichermedium)))

|#


