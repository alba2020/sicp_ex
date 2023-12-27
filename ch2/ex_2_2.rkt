;ex 2.3-2.3
#lang sicp

(define (average x y)
  (/ (+ x y) 2.0))

(define (double x) (* 2 x))
(define (square x) (* x x))

; ------ point ----------
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; ------ segment ---------
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;-----------------
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))
(define (length-segment s)
  (let ((dx (- (x-point (end-segment s)) (x-point (start-segment s))))
        (dy (- (y-point (end-segment s)) (y-point (start-segment s)))))
    (sqrt (+ (* dx dx) (* dy dy)))))
  
;--------------
(define p1 (make-point 1 2))
(print-point p1)
(define p2 (make-point 4 5))
(print-point p2)

(define seg (make-segment p1 p2))
(define midpoint (midpoint-segment seg))
(print-point midpoint)

;(newline)
;(length-segment (make-segment (make-point 1 1) (make-point 4 5)))

;---------- rectangle by two sides ----------
;(define (make-rec seg-left seg-top)
;  (cons seg-left seg-top))
;(define (height rect)
;  (length-segment (car rect)))
;(define (width rect)
;  (length-segment (cdr rect)))

;------------- rectangle by point, lengths of sides and angle
;(define (make-rec point alpha height width)
;  (cons (cons point alpha) (cons height width)))
;(define (height rec)
;  (car (cdr rec)))
;(define (width rec)
;  (cdr (cdr rec)))

; ----- rectangle by diagonal and angle
(define (make-rec diag angle)
  (cons diag angle))
(define (height rec)
  (* (length-segment (car rec))
     (sin (cdr rec))))
(define (width rec)
  (* (length-segment (car rec))
     (cos (cdr rec))))

;-------------------
(define (area rec)
  (* (height rec) (width rec)))
(define (perimeter rec)
  (double (+ (height rec) (width rec))))

;(define left (make-segment (make-point 1 1) (make-point 1 4)))
;(define top (make-segment (make-point 4 1) (make-point 4 4)))
;(define r (make-rec left top))
;(area r)
;(perimeter r)

;(define r (make-rec (make-point 1 1) 0 10 20))
;(area r)
;(perimeter r)

(define d (make-segment (make-point 1 1) (make-point 4 4)))
(define r (make-rec d 0.7))
(newline)
(height r)
(width r)
(sqrt (+ (square (height r)) (square (width r))))
(area r)
(perimeter r)

