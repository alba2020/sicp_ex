#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 11))
(car (cons 2 12))

(cdr (cons 1 11))
(cdr (cons 2 12))