#lang racket

(require racket/trace)

#|
1.1
Wir nutzen hierbei die hash-table Funktion von Racket,
da diese bereits eine komfortabele Lösung zu Eingabe und Abruf von key-value paaren bietet.
|#
(define tafel (hash #\A "Alpha" #\B "Bravo" #\C "Charlie" #\D "Delta" #\E "Echo" #\F "Foxtrot" #\G "Golf" #\H "Hotel" #\I "India" #\J "Juliet" #\K "Kilo" #\L "Lima" #\M "Mike" #\N "November" #\O "Oscar" #\P "Papa" #\Q "Quebec" #\R "Romeo" #\S "Sierra" #\T "Tango" #\U "Uniform" #\V "Victor" #\W "Whiskey" #\X "X-ray" #\Y "Yankee" #\Z "Zulu" #\0 "Nadazero" #\1 "Unaone" #\2 "Duotwo" #\3 "Terrathree" #\4 "Carrefour" #\5 "Pentafive" #\6 "Soxisix" #\7 "Setteseven" #\8 "Oktoeight" #\9 "Novonine" #\, "Decimal" #\. "Stop"))

;1.2
(define (get-alphabet-code char)
  (hash-ref tafel char))

;1.3
(define (string->alphabetcode input)
  (define (process-list list)
    (if (empty? list)
        list
        (cons (get-alphabet-code (car list))
              (process-list (cdr list)))))
  (trace process-list)
  ;check if input is a string
  (if (string? input)
      ;transform string to list of chars and work through the list
      (process-list (string->list input))
      #f))

(trace string->alphabetcode)

(string->alphabetcode "TEST12")

#|
2.1
|#
(require se3-bib/flaggen-module)

(define flaggen
  '((#\A . A)
    (#\B . B)
    (#\C . C)
    (#\D . D)
    (#\E . E)
    (#\F . F)
    (#\G . G)
    (#\H . H)
    (#\I . I)
    (#\J . J)
    (#\K . K)
    (#\L . L)
    (#\M . M)
    (#\N . N)
    (#\O . O)
    (#\P . P)
    (#\Q . Q)
    (#\R . R)
    (#\S . S)
    (#\T . T)
    (#\U . U)
    (#\V . V)
    (#\W . W)
    (#\X . X)
    (#\Y . Y)
    (#\Z . Z)
    (#\0 . Z0)
    (#\1 . Z1)
    (#\2 . Z2)
    (#\3 . Z3)
    (#\4 . Z4)
    (#\5 . Z5)
    (#\6 . Z6)
    (#\7 . Z7)
    (#\8 . Z8)
    (#\9 . Z9)))

#|
2.2
|#
(define (get-flag char)
  (eval (cdr (assoc char flaggen))))

#|
2.3
TODO: funzt noch nicht
|#

(define (string->flags input)
  (define (process-list list)
    (if (empty? list)
        '()
        (cons (get-flag (car list))
              (process-list (cdr list)))))
  (trace process-list)
  (process-list (string->list input)))

;funktioniert nur in der Konsole (?)
;(string->flags "SOS")

#|
3
|#
(require racket/gui/base)

;(play-sound "Morse-A.wav" #f)

;3.1

(define sounds
  '((#\A . "Morse-A.wav")
    (#\B . "Morse-B.wav")
    (#\C . "Morse-C.wav")
    (#\D . "Morse-D.wav")
    (#\E . "Morse-E.wav")
    (#\F . "Morse-F.wav")
    (#\G . "Morse-G.wav")
    (#\H . "Morse-H.wav")
    (#\I . "Morse-I.wav")
    (#\J . "Morse-J.wav")
    (#\K . "Morse-K.wav")
    (#\L . "Morse-L.wav")
    (#\M . "Morse-M.wav")
    (#\N . "Morse-N.wav")
    (#\O . "Morse-O.wav")
    (#\P . "Morse-P.wav")
    (#\Q . "Morse-Q.wav")
    (#\R . "Morse-R.wav")
    (#\S . "Morse-S.wav")
    (#\T . "Morse-T.wav")
    (#\U . "Morse-U.wav")
    (#\V . "Morse-V.wav")
    (#\W . "Morse-W.wav")
    (#\X . "Morse-X.wav")
    (#\Y . "Morse-Y.wav")
    (#\Z . "Morse-Z.wav")))

;3.2

(define (char-morse char)
 (play-sound (eval (cdr (assoc char sounds)))#f))

;3.3

