#lang racket
(define (factorial n)
  (if (> n 0)
      (* n (factorial (- n 1)))
      1))

(define (fact n)
  (define (fact-iter product counter)
    (if (= counter n)
        product
        (fact-iter (* product (+ counter 1)) (+ counter 1))))
  (fact-iter 1 1))

;(fact 50)
;(factorial 30)