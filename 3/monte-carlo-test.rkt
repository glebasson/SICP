#lang racket
(define (rand-update x)
  (remainder (+ (* 9309 x) 49297) 233280))

(define random-init 110)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(rand)
(rand)
(rand)


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
           (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(estimate-pi 300)

; exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (estimateintegral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (define (S x1 x2 y1 y2)
    (* (- x2 x1) (- y2 y1)))
  (* (monte-carlo trials experiment) (S x1 x2 y1 y2)))
(define (in-cycle? x y)
  (<= (+ (* x x) (* y y)) 1))

(estimateintegral in-cycle? -1 1 -1 1 10000)

; exercise 3.6
(define new-rand
  (let ((x random-init))
    (define (dispatch msg)
      (cond ((eq? msg 'generate)
             (set! x (rand-update x))
             x)
            ((eq? msg 'reset)
             (lambda (new-value)
               (set! x new-value)))))
    dispatch))

(new-rand 'generate)
(new-rand 'generate)
((new-rand 'reset) 110)
(new-rand 'generate)
