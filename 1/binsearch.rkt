#lang racket
(define (average a b)
  (/ (+ a b) 2))
(define (abs a)
  (if (> a 0)
      a
      (- a)))

(define (close-enough? x y)
  (< (abs (- x y)) 0.01))

(define (positive? a)
  (> a 0))
(define (negative? a)
  (< a 0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "differnet arg signs " a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

; fiexd point
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display "guess: ")
      (display guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point cos 1.0)
;(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

;(define (sqrt x)
;  (fixed-point (lambda (y) (average y  (/ x y))) 1.0))
;(sqrt 2)

; exercise 1.35
; golden ratio evaluation
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(golden-ratio)

; exercise 1.36 x^x=1000
; (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1) ; 74 w
; (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.1) ; 26 w

; exercise 1.37
(define (cont-frac n d k)
  (define (cont-frac-iter k i)
    (if (= k 1)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-iter (- k 1) (+ i 1))))))
  (cont-frac-iter k 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

; exercise 1.38
(define (d i)
  (if (or (= (remainder i 3) 1) (= (remainder i 3) 0))
      1.0
      (* 2.0 (ceiling (/ i 3)))))
(cont-frac (lambda (i) 1.0)
           d
           100)

(define (square x) (* x x))

; exercise 1.39
(define (tan-cf x k)
  (/ (cont-frac (lambda (i) (-(square x)))
                (lambda (i) (+ 1 (* 2 (- i 1))))
                k)
     (- x)))
(tan-cf 1.0
        500)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(sqrt 2)

(define (deriv g)
  (lambda (x)
  (/ (- (g (+ x dx)) (g x))
     dx)))
(define dx 0.0001)
(define (cube x) (* x x x))
((deriv cube) 5)

; Newton`s method
(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

(define (newtons-sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
(newtons-sqrt 2)

