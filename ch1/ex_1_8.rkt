#lang sicp
; cube root

(define (square x) (* x x))

(define (div-by-3 x)
  (/ x 3.0))

(define (improve guess x)
  (div-by-3 (+ (/ x (square guess))
               (* guess 2.0))))

(define (cr-iter guess old x)
  (if (good? guess old x)
      guess
      (cr-iter (improve guess x)
               guess
               x)))

(define (delta x y)
  (abs (- x y)))

(define (good? guess old x)
  (< (/ (delta guess old) old) 0.0001))

(define (cr x)
  (cr-iter 2.0 1.0 x))
