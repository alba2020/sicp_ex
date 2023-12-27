#lang sicp

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (if (not (= (remainder z 2) 0))
      0
      (+ 1 (car (/ z 2)))))

(define (cdr z)
  (if (not (= (remainder z 3) 0))
      0
      (+ 1 (cdr (/ z 3)))))

(define z (cons 3 4))
z
(car z)
(cdr z)

(car (cons 12 14))
(cdr (cons 12 14))
