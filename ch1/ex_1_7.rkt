#lang sicp

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;---- improved version -----

(define (sqrt-iter-imp guess old x)
  (if (good-imp? guess old x)
      guess
      (sqrt-iter-imp (improve guess x)
                guess
                x)))

(define (delta x y)
  (abs (- x y)))

(define (good-imp? guess old x)
  (< (/ (delta guess old) old) 0.0001))

(define (sqrt-imp x)
  (sqrt-iter-imp 2.0 1.0 x))
