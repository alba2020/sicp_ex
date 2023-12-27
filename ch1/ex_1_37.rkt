; 1.37-1.38
#lang sicp

; recursive
;(define (cont-frac n d k)
;  (define (term i)
;    (if (= i k)
;        (/ (n i) (d i))
;        (/ (n i)
;           (+ (d i) (term (+ i 1))))))
;  (term 1))

;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           3)

(define (one i) 1.0)

(define (g k)
  (cont-frac one one k))

; iterative
(define (cont-frac n d k)
  (define (iter k a)
    (if (= k 0)
        a
        (iter (- k 1)
              (/ (n k)
                 (+ (d k) a)))))
  (iter k 0))

(define (div a b)
  (/ (- a (remainder a b))
     b))

(define (double x) (* x 2))
(define (inc x) (+ x 1))
(define (ed k)
  (if (= (remainder k 3) 2)
      (double (inc (div k 3)))
      1))

(+ 2 (cont-frac one ed 20))
(exp 1)

