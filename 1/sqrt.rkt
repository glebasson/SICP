#lang racket
(define (abs x) (if (> x 0) x (- x)))
(abs 20)
(abs -10)
(define (square x)
  (* x x))
(define (goodenough? guess x) (< (abs (- (square guess) x))
                             0.0001))

(define (goodenough2? guess prevguess)
  (< (abs (- guess prevguess))
     0.00001))


(define (sqrt-iter guess x)
  (if (goodenough2? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

;(define (improve guess x)
;  (average guess (/ x guess)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))


(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 81)