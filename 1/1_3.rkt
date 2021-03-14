#lang racket
(define (cube x) (* x x x))

; naive implementation
;(define (sum-integers a b)
;  (if (> a b)
;      0
;      (+ a (sum-integers (+ a 1) b))))

;(define (sum-cubes a b)
;  (if (> a b)
;      0
;      (+ (cube a) (sum-cubes (+ a 1) b))))

;(sum-integers 1 5)
;(sum-cubes 1 3)

;(define (pi-sum a b)
;  (if (> a b )
;      0
;      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

; general implementation
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc i) (+ i 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 3)
(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))


(define (pi-sum a b)
  (define (pi-next i) (+ i 4))
  (define (pi-term x)
    (/ 1 (* x (+ x 2))))
  (sum pi-term a pi-next b))

; find pi
(* (pi-sum 1 1000) 8)

; integral
(define (integral f a b dx)
  (define (integral-next x) (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2)) integral-next b)))
(integral cube 0 1 0.01)


; exercise 1.29
; integration by Simpson`s rule
(define (integral-simpson f a b n)
  (define (h) (/ (- b a) n))
  (define (f2 x) (* (f x) 2))
  (define (f4 x) (* (f x) 4))
  (define (next x)
    (+ x (* (h) 2)))
  (* (/ (h)
        3.0)
     (+ a
        b
        (sum f2 (+ a (* 2 (h))) next (- b (* 2 (h))))
        (sum f4 (+ a (* 1 (h))) next (- b (* 1 (h)))))))
(integral-simpson cube 0 1 1000)




 