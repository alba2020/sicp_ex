#lang sicp

;Exercise 1.46.  Several of the numerical methods described in this chapter are instances
;of an extremely general computational strategy known as iterative improvement. Iterative improvement
;says that, to compute something, we start with an initial guess for the answer, test if the guess is
;good enough, and otherwise improve the guess and continue the process using the improved guess as the
;new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for
;telling whether a guess is good enough and a method for improving a guess. Iterative-improve should
;return as its value a procedure that takes a guess as argument and keeps improving the guess until it
;is good enough. Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of
;section 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001)) ;good-enough?
                      (lambda (guess) (average guess (/ x guess)))) ;improve
   1.0))

(sqrt 625)

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) 0.00001))
                      (lambda (guess) (f guess)))
   first-guess))

;(define pi (* 4 (atan 1.0)))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)