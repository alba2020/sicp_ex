#lang sicp

(define (fib2 n)
  (fib-iter2 1 0 n))

(define (fib-iter2 a b count)
  (if (= count 0)
      b
      (fib-iter2 (+ a b) a (- count 1))))

; ---------------------------------
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (next-p p q)
  (sum-of-squares p q))

(define (next-q p q)
  (+ (* 2 p q) (square q)))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (next-p p q); compute p'
                   (next-q p q); compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
