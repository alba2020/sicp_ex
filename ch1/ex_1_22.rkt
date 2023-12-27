; 1.22 - 1.28
#lang sicp

(define (square x)
  (* x x))

(define (rnd x)
  (define max 4000000000)
  (if (> x max) (random max) (random x)))

; ------------ prime test sqrt(n) -----------
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
; ---------- fermat test ----------------
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (rnd (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Carmichael
(define (fermat-test-full n)
  (define (test n a)
    ;    (display n)
    ;    (display " ")
    ;    (display a)
    ;    (newline)
    (cond ((= a 1) true)
          ((= (expmod a n n) a) (test n (- a 1)))
          (else false)))
  (test n (- n 1)))

; --------------- timed test --------------------
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; --------- search for primes -----------
(define (search-for-primes min max)
  (if (or (< min max) (= min max))
      (test-and-next min max)))

(define (test-and-next min max)
  (timed-prime-test min)
  (search-for-primes (+ min 2) max))

; ---------------------------------------------
; non-trivial square root modulo 1
(define (srm? x n)
  (= (remainder (square x) n) 1))

(define (non-trivial? x n)
  (and (srm? x n)
       (not (= x 1))
       (not (= x (- n 1)))))

(define (find-roots min max n)
  (if (or (< min max) (= min max))
      (next-iter min max n)))

(define (next-iter min max n)
  (if (non-trivial? min n) (print min))
  ;(print min)
  (find-roots (+ min 1) max n))

(define (print x)
  (display x)
  (display " "))

; -------- miller rabin test ------------
(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (next2 base exp m))
        (else
         (remainder (* base (expmod2 base (- exp 1) m))
                    m))))

(define (next2 base exp m)
  (define rem
    (remainder (square (expmod2 base (/ exp 2) m))
               m))
  (print rem)
  (if (non-trivial? rem m) (display "<- "))
  (if (non-trivial? rem m)
      0
      rem))

(define (mr-test n)
  (define (try-it a)
    (= (expmod2 a (- n 1) n) 1))
  (try-it (+ 1 (rnd (- n 1)))))

(define (mr-test-full n)
  (define (test n a)
    (cond ((= a 1) true)
          ((= (expmod a (- n 1) n) 1) (test n (- a 1)))
          ;((= (expmod a (- n 1) n) 0) false)
          (else false)))
;  (define (next n a val)
;    (if (= val 1) (test n(- a 1)) ))
  (test n (- n 1)))

;(cond
;  ((display 1111) 1)
;  ((display 2222) 2))

