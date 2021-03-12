#lang racket
(define  (fib n)
  (cond ((= n 0) n)
        ((= n 1) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))
        )
  )


(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
                 (cc amount (- kinds-of-coins 1))))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


; exercise 1.11
; recursive version
;(define (f n)
;  (if (< n 3)
;      n
;      (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))

; iterating version
(define (f n)
  (define (f-iter product prev prev2 counter max-counter)
    (if (= counter max-counter)
        product
        (f-iter (+ product prev prev2) product prev (+ counter 1) max-counter)))
  (if (< n 3)
      n
      (f-iter 2 1 0 2 n))
  )

(f 6)
; 20

; exercise 1.12
; Pascal`s triangle`s elements evaluation
(define (P line index)
  (if (and (= line 1) (= index 1))
      1
      (if (or (< index 1) (> index line))
          0
          (+ (P (- line 1) (- index 1))
             (P (- line 1) index)))))

(P 4 2)


; exercise 1.13
; iterative expt
(define (fast-expt b n)
  (define (iter a b n)
    (if (= n 0)
        a
        (if (= (remainder n 2) 0)
            (iter (* a a) b (/ n 2))
            (iter (* a b) b (- n 1)))))

  (if (= n 0)
      1
      (iter b b (- n 1))))

(fast-expt 3 5)
; 243

; exercise 1.17
; fast product with barrel-shifers
;(define (fast-product a n)
;  (define (double n) (* n 2))
;  (define (half n) (/ n 2))
;  (if (= n 1)
;      a
;      (if (= (remainder n 2) 0)
;           (fast-product (double a) (half n))
;           (+ a (fast-product a (- n 1))))))

;(fast-product 5 10)
; 50

; exercise 1.18
; fast iterative product with barrel-shifters
(define (fast-iter-product a n)
  (define (double n) (* n 2)) ; overwrite *2
  (define (half n) (/ n 2)) ; overwrite /2
  (define (iter p a n)
    (if (= n 0)
        p
        (if (= (remainder n 2) 0)
            (iter (double p) a (half n))
            (iter (+ p a) a (- n 1)))))
  (if (= n 0)
      0
      (iter a a (- n 1))))
(fast-iter-product 4 5)
; 20

; exercise 1.19
; fibonachi for log(n)
(define (fib-fast n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((= (remainder count 2) 0)
         (fib-iter a b
                   (+ (* p p)
                      (* q q))
                   (+ (* q q)
                      (* 2 q p))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))
        ))
(fib-fast 7)
