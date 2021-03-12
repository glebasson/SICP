#lang racket
; greatest common divider
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 27 81)

(define (square n)
  (* n n))

; prime check
(define (smallest-divisor n)
  (find-divisor n 2))

; exercise 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; (prime? 993275708508481636471678134843212298085177)

(define (even? a) (= (remainder a 2) 0))

; Ferma test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder base (* (expmod base (- exp 1) m) m)))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
(current-milliseconds)

; exercise 1.23
; timed tests
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-process-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
    (report-prime (- (current-process-milliseconds) start-time)
    ))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (define (iter a b)
    (cond ((< a b)
           (timed-prime-test a)
           (iter (+ a 2) b))))
  (if (even? a)
      (iter (+ 1 a) b)
      (iter a b)))

;(search-for-primes 10000 100000)
; my computer is too fast((


; exercise 1.27
(define (karlmike-test n)
  (define (iter n a)
    (if (< a n)
        (if (= a (expmod a n n))
            (iter n (+ a 1))
            false)
        true))
  (iter n 2))

(karlmike-test 1105)

