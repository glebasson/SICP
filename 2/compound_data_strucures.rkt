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
  (display ")")
  (newline))

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

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(define even (list 0 2 4 6))
(length odds)
(define squares (list 0 1 4 9 16))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append squares odds)

; exercise 2.17
(define (last-pair list)
      (if (null? (cdr list))
          (car list)
          (last-pair (cdr list))))
(last-pair (list 1))

(define (del-last-pair list)
  (if (null? (cdr list))
      null
      (cons (car list) (del-last-pair (cdr list))))
  )
(del-last-pair (list 1))


; exercise 2.18
(define (reverse list1)
  (if (null? list1)
      (list)
      (cons (last-pair list1)
        (reverse (del-last-pair list1))))
  )
(reverse (list 1 2 3 4 5))
(define (even? x)
  (= 0 (remainder x 2)))
(define (odd? x)
  (= 1 (remainder x 2)))

(define (filter f list1)
  (if (null? list1)
      (list)
      (if (f (car list1))
          (cons (car list1) (filter f (cdr list1)))
          (filter f (cdr list1)))
      ))
(filter (lambda (x) (> x 0)) (list -3 -2 -1 0 1 2 3 4))

(define (same-parity par . l)
  (define (test x)
    (if (even? par)
        even?
        odd?))
  (filter test (cons par l)))

(same-parity 16 2 3 4 5 6)

(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 25))
(map (lambda (x) (* x x)) (list 1 2 3 4))

; exe 2.23
(define (for-each f list1)
  (unless (null? list1)
    (f (car list1))
    (for-each f (cdr list1))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
