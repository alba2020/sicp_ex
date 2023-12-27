#lang sicp

; y - row x - column
(define (pas y x)
  (if (or (= x 1) (= x y))
      1
      (+ (pas (- y 1) (- x 1))
         (pas (- y 1) x))))
