#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (max a b)
  (if (> a b) a b))

(define (min a b)
  (if (< a b) a b))

(define (f a b c)
  (sum-of-squares (max a b)
                  (max (min a b) c)))
  