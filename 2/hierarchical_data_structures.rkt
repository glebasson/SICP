#lang racket
(define x (cons (list 1 2)
                (cons (list 3 4) (list))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))
(count-leaves x)
(list 1 (list 2 (list 3 4)))
(define xx (list 1 2 3))
(define y (list 4 5 6))
(append xx y)
(cons xx y)
(list xx y)


;(reverse (list 1 2 3 4))
; exercise 2.27
(define X (list (list 1 2) (list 3 4)))
(define (deep-reverse tree)
  (if (pair? tree)
      (reverse (map deep-reverse tree))
      tree))

(deep-reverse X)
(append (list 1 2 3 4 5) (list 2 3 4 5))

; exercise 2.28
(define nil (quote ()))
(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? tree)
         (append (fringe (car tree)) (fringe (cdr tree))))
        (else
         (list tree)))
  )

(list X X)
(fringe (list X X))

; exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
   tree)
  )
(tree-map (lambda (y) (* y y)) (list X X))

; exercise 2.32
; TODO
;(define (subsets s)
;  (if (null? s)
;      (list nil)
;      (let ((rest (subsets (cdr s))))
;        (append rest (map <??> rest)))))



