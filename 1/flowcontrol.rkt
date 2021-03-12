#lang racket

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

(define (sum-squeres x y) (+ (* x x) (* y y)))
(sum-squeres 2 3)
(define (foo x y z) (cond ((and (> x y) (> z y)) (sum-squeres x z))
                          ((and (> y x) (> z x)) (sum-squeres y z))
                          ((and (> x z) (> y z)) (sum-squeres x y))))


(foo 5 6 2)

(define (new-if predicate then-clause else-clause)
(cond (predicate then-clause)
(else else-clause)))

(define (sqrt-iter guess x)
(new-if (good-enough? guess x)
guess
(sqrt-iter (improve guess x)
x)))

;(define (sqrt-iter guess x)
;(if (good-enough? guess x)
;guess
;(sqrt-iter (improve guess x)
;x)))

(define (square x) (* x x))

(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
(average guess (/ x guess)))

(define (average x y)
(/ (+ x y) 2))

(define (sqrt x)
(sqrt-iter 1.0 x))

(sqrt 9)