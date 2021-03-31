#lang racket
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
                   balance)
      "Not enought money on the account!"))

(withdraw 50)
(withdraw 30)
(withdraw 100)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                       balance)
                 "Not enought money on the account!"))))

(new-withdraw 10)
(new-withdraw 10)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enought money on the account!")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 150))

(W1 10)
(W2 50)

; exercise 3.3
(define (make-account secret balance)
  (define (authenticated? pswd)
    (eq? secret pswd))
  (define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
                   balance)
      "Not enought money on the account!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pswd m)
    (if (authenticated? pswd)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "UNKNOWN CALL"
                       m)))
        (lambda (x) (display "WRONG PASSWORD!\n"))))
  dispatch)

(define acc (make-account 'secret-password 100))
((acc 'secret-password 'withdraw) 50)
((acc 'secret-password 'withdraw) 60)
((acc 'wrong-password 'deposit) 40)
((acc 'secret-password 'withdraw) 60)

; exercise 3.1
(define (make-accumulator value)
  (lambda (x)
    (begin (set! value (+ value x))
           value)))
(define A (make-accumulator 0))
(A 10)
(A -5)

; exercise 3.2
(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?)
      count)
    (define (reset-count)
      (set! count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else (begin (set! count (+ count 1))
                         (f m))
                  )))
      dispatch))

(define s (make-monitored sqrt))
(s 100)
(s 100)
(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)

