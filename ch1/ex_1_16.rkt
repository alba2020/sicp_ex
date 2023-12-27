#lang sicp

; test
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; -------------

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (exp b n)
  (iter b n 1))

(define (iter b n a)
  (cond ((= n 0) a)
        ((even? n)(iter (square b)
                        (- (/ n 2) 1)
                        (* a (square b))))
        (else (iter b
                    (- n 1)
                    (* b a)))))

