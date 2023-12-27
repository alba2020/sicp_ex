#lang sicp

(define (m2 x) (* 2 x))
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda(x) (f ((n f) x)))))

(define (sum m n)
  (lambda (f) (lambda(x) ((m f) ((n f) x)))))

;(define one (add-1 zero))
;(define two (add-1 (add-1 zero)))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (time m)
  (define (times n f x)
    (if (= n 1)
        (f x)
        (times (- n 1) f (f x))))
  (lambda (f) (lambda (x) (times m f x))))

(define test2 (two m2))
(test2 2)

(define one-b (time 1))
(define test-b (one-b m2))
(test-b 2)

(define two-c (time 2))
(define test-c (two-c m2))
(test-c 2)

(define three-c (time 3))
(define test3 (three-c m2))
(test3 2)
