#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point cos 1.0)
;(fixed-point (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0)
;(fixed-point (lambda (y) (+ (sin y) (cos y)))
;             1.0)

;(define (sqrt x)
;  (fixed-point (lambda (y) (average y (/ x y)))
;               1.0))

;(define (golden-f x)
;  (+ 1 (/ 1 x)))

;(fixed-point golden-f 1.0)

;(fixed-point (lambda (x) (/ (log 1000) (log x)))
;             2.0)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)