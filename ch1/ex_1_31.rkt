; 1.31-1.33
#lang sicp

(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (identity x) x)
(define (square x) (* x x))
(define (odd? x) (= (remainder x 2) 1))
(define (even? x) (= (remainder x 2) 0))

; ---------- prime? ---------
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (next x)
    (if (= x 2) 3 (+ x 2)))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (smallest-divisor n)))

; ------------- gcd ---------
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; sum-recursive
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))


; sum-iterative
;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (+ (term a) result))))
;  (iter a 0))

; product-recursive
;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;         (product term (next a) next b))))

; product-iterative
;(define (product term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (* (term a) result))))
;  (iter a 1))

; accumulate-recursive
;(define (accumulate combiner null-value term a next b)
;  (if (> a b)
;      null-value
;      (combiner (term a)
;                (accumulate combiner null-value term (next a) next b))))

(define (print x y)
  (display x)
  (display " ")
  (display y)
  (newline))

; acc-iter
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    ;(print a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; sum-acc
(define (sum term a next b)
  (accumulate + 0 term a next b))

; product-acc
(define (product term a next b)
  (accumulate * 1 term a next b))

; filtered acc-iter
(define (filtered-accumulate combiner null-value term a next b test)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term-with-test a) result))))
  (define (term-with-test a)
    (if (test a) (term a) null-value))
  (iter a null-value))

(define (sum-odds a b)
  (filtered-accumulate + 0 identity a inc b odd?))

(define (sum-square-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

; relatively primes less than n
(define (rp n)
  (define (test? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) test?))

; -------------------------
(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (factorial a)
  (define (next x) (+ x 1))
  (product identity 1 next a))

(define (pi4 a b)
  (define (next x) (+ x 2))
  (define (term n)
    (/ (* (+ n 1.0) (+ n 3))
       (square (+ n 2))))
  (product term a next b))

; test
(sum-cubes 1 10) ;3025
(sum-integers 1 10) ;55
(factorial 5) ;120
(* (pi4 1 1000) 4) ;~pi

(sum-square-primes 2 10); 2 3 5 7 = 4 9 25 49 = 87
(rp 10); 1 3 7 9 = 189


