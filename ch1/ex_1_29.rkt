#lang sicp

(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (identity x) x)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (odd? x)
  (= (remainder x 2) 1))

(define (even? x)
  (= (remainder x 2) 0))


(define (s-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    ;(display k)
    ;(display " ")
    (f (+ a (* k h))))
  (define (yy k)
    (cond ((= k 0) (y k))
     ((= k n) (y k))
     ((odd? k) (* 4 (y k)))
     ((even? k) (* 2 (y k)))))
  (define (next n) (+ n 1))
  (* (/ h 3)
     (sum yy 0 next n)))
