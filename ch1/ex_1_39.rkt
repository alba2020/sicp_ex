#lang sicp

(define (cont-frac n d k)
  (define (iter k a)
    (if (= k 0)
        a
        (iter (- k 1)
              (/ (n k)
                 (+ (d k) a)))))
  (iter k 0))

(define (dec x) (- x 1))
(define (double x) (* x 2))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1.0) x (* x x -1.0)))
  (define (d k)
    (dec (double k)))
  (cont-frac n d k))

(tan-cf 1 9)
(tan 1)

(tan-cf 2 12)
(tan 2)
