#lang sicp

(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (log2 x)
  (/ (log x) (log 2)))

; ------- fixed point ----------------
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (rep g n)
    (cond ((= n 1) g)
          (else (rep (compose f g) (- n 1)))))
  (rep f n))


(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (cube-root x)
  (fixed-point-of-transform (lambda (y) (/ x (square y)))
                            average-damp
                            1.0))

;(define (rt n x damps)
;  (fixed-point-of-transform (lambda(y) (/ x (expt y (- n 1))))
;                            (repeated average-damp damps)
;                            1.0))

(define (root-n n x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (floor (log2 n)))
                            1.0))
