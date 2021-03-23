#lang racket
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; (enumerate-interval 0 100)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; exe 2.33
;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
;(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2 8 3 4) (list 1 2 3 4))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3 4))

; exe 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

; exe 2.35
; that exercise isn`t intelegent! Just enumerate tree and than find length. No maps or accumulate is involved
;(define (count-leaves t)
;  (accumulate h??i 0 (map (lambda (x) 1) t)))

 (define (select-cars sequence) 
   (map car sequence)) 
  
 (define (select-cdrs sequence) 
   (map cdr sequence)) 
  
 ;; Test 
 (define t (list (list 1 2 3) (list 40 50 60) (list 700 800 900))) 
 (select-cars t) 
 (select-cdrs t) 

; exe 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init
                        (map car seqs))
      (accumulate-n op init
                    (map cdr seqs)))))

(accumulate-n + 0 t)

; exe 2.37
; Matrix algebra
(define M (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
; Test
(dot-product (list 1 2 3) (list 1 2 3))

(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v)) m))
; Test
(matrix-*-vector M (list 1 2 3 4))

(define (transpose mat)
  (accumulate-n cons '() mat))
; Test
(transpose M)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector n x)) m)))
(matrix-*-matrix M M)

; exe 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define nil '())
(accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(accumulate list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
; associativity should be presist

; exe 2.39
;(define fold-right accumulate)
;(define (reverse sequence)
;  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))
(reverse (list 1 2 3))
