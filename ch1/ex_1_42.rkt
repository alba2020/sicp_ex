; 1.42-1.44
#lang sicp

(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (odd? x) (= (remainder x 2) 1))
(define (even? x) (= (remainder x 2) 0))
(define (id x) x)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (define (rep g n)
    ;(display n)
    ;(display " ")
    (cond ((= n 1) g)
          ;((even? n) (rep (compose g g) (- (/ n 2) 1)))
          (else (rep (compose f g) (- n 1)))))
  (rep f n))

((repeated inc 11) 1)
((repeated square 2) 5)

(define dx 0.0001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

