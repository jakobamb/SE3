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
