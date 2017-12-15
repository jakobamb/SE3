#lang racket

(require racket/trace)

;Aufgabe 1
;allgemein rekursiv
(define (zaehlen x xs)
  (if (null? xs)
      0
      (if (equal? (car xs) x)
          (+ 1 (zaehlen x (cdr xs)))
          (zaehlen x (cdr xs)))))

;endrekursiv
(define (zaehlen_end x xs [sum 0])
  (if (null? xs)
      sum
      (if (equal? (car xs) x)
          (zaehlen_end x (cdr xs) (add1 sum))
          (zaehlen_end x (cdr xs) sum))))

;higher-order
(define (zaehlen_ho x xs)
  (foldl (lambda (a b)
           (if (equal? a x)
               (add1 b)
               b))
         0
         xs))

(trace zaehlen zaehlen_end zaehlen_ho)

(zaehlen 3 (list 2 5 7 3 56 3 6456 3 543 42))
(zaehlen_end 3 (list 2 5 7 3 56 3 6456 3 543 42))
(zaehlen_ho 3 (list 2 5 7 3 56 3 6456 3 543 42))
