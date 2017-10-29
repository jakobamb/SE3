
#lang racket

;#1.1
(define (degrees->radians d)
  (* d (/ pi 180)))
;function alias
(define dr degrees->radians)

(define (radians->degrees r)
  (* r (/ 180 pi)))
;function alias
(define rd radians->degrees)


;#1.2
(define (my-acos a)
  (atan (/
         (- (sqrt 1) (cos a))
         (cos a))))

;#1.3
(define (nm->km nm)
  (* nm 1.852))

;#2.1
(define (distanzAB a_gb a_gl b_gb b_gl)
  (nm->km (* 60
             (radians->degrees (acos (+ (* (sin (degrees->radians a_gb))
                                           (sin (degrees->radians b_gb)))
                                        (* (cos (degrees->radians a_gb))
                                           (cos (degrees->radians b_gb))
                                           (cos (abs (- (degrees->radians a_gl)
                                                        (degrees->radians b_gl)))))))))))
;Oslo -> Hongkong = 8589.412217586058 km
(distanzAB 59.93 10.75 22.2 114.1)
;SF -> Honolulu = 3844.688050487052 km
(distanzAB 37.75 -122.45 21.32 -157.83)
;Osterinsel -> Lima = 3757.622218810054 km
(distanzAB -27.10 -109.4 -12.1 -77.05)

;#2.3
(define (Grad->Himmelsrichtung grad ) (if (< grad 30)
                                          "N"
                                          (if (< grad 60)
                                              "NNE"
                                              (if (< grad 90)
                                                  "ENE"
                                                  (if (< grad 120)
                                                      "E"
                                                      (if (< grad 150)
                                                          "EES"
                                                          (if (< grad 180)
                                                              "SES"
                                                              (if (< grad 210)
                                                                  "S"
                                                                  (if (< grad 240)
                                                                      "SSW"
                                                                      (if (< grad 270)
                                                                          "WSW"
                                                                          (if (< grad 300)
                                                                              "W"
                                                                              (if (< grad 330)
                                                                                  "WWN"
                                                                                  (if (< grad 360)
                                                                                      "NWN"
                                                                                      "Invalid Input"
                                          )))))))))))))

(define (Himmelsrichtung->Grad HR) (if (string-locale=? HR "N") 0
                                       (if (string-locale=? HR "NNE") 30
                                           (if (string-locale=? HR "ENE") 60
                                               (if (string-locale=? HR "E") 90
                                                   (if (string-locale=? HR "EES") 120
                                                       (if (string-locale=? HR "SES") 150
                                                           (if (string-locale=? HR "S") 180
                                                               (if (string-locale=? HR "SSW") 210
                                                                   (if (string-locale=? HR "WSW") 240
                                                                       (if (string-locale=? HR "W") 270
                                                                           (if (string-locale=? HR "WWN") 300
                                                                               (if (string-locale=? HR "NWN") 330
                                                                                       "Invalid Input"
                                       )))))))))))))
