#lang sicp

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))
         
(define (iter a b c n)
  (if (= n 0)
      c
      (iter (next a b c) a b (- n 1))))

(define (next a b c)
  (+ a (* 2 b) (* 3 c)))

(define (f-iter n)
  (if (< n 3)
      n
      (iter 2 1 0 n)))

;(define (it a b c n target)
;  (cond ((< target 3) target)
;        ((= n target) a)
;        (else (it (next a b c) a b (+ n 1) target))))

;(define (f-iter n)
;  (it 2 1 0 2 n))
