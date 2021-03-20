#lang racket
(define x (cons 1 2))
(car x)
(cdr x)

(define (average x y)
  (/ (+ x y) 2))

; exercise 2.2
(define (make-point a b)
  (cons a b))
(define (x-point c) (car c))
(define (y-point c) (cdr c))

(define (make-segment a b)
  (cons a b))
(define (start-segment c) (car c))
(define (end-segment c) (cdr c))

(define (midpoint-segment z)
  (make-point (average (x-point (start-segment z)) (x-point (end-segment z)))
              (average (y-point (start-segment z)) (y-point (end-segment z)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment (make-segment (make-point -1 -1) (make-point 1 1))))

; naive implementation
;(define (make-rect down-left up-right)
;  (cons x y))
;
;(define (perimeter rect)
;  (* 2 (+ (width rect) (height rect))))
;
;(define (area rect)
;  (* (width rect) (height rect)))

