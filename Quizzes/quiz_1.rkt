#lang racket

;; quiz #1 on problem 2.1 from the book, implementing nnint2dec and dec2nnint for bigits.
;; Numbers are represented in base N (large N)
;; The reprsentation uses a list of numbers such that each number is in 0...N-1 (called bigits)
;; zero, iszero?, succ, pred, nnint2dec, dec2nnint plus sub

;; An nnint is  either ...
;;     '()
;;     (cons r '(q))
;; where r is greater than or equal to zero and less than N,
;; and q is q * N where N is the base (big N)

;;(define (zero) '(0))

;; (define (iszero? nnint)
;;   (and 
;;     (= (car nnint) 0)
;;     (= (length nnint) 1)))

(define (zero) '() )

(define (iszero? nnint N) (null? nnint))

;;this won't work
(define (succ nnint N) 
  (cond
    [(< (car nnint) (sub1 N)) (add1 (car nnint))]
    [(= (car nnint) (sub1 N)) (succ (cdr nnint) N)]
    [(null? cdr) (cons 1 nnint)]))

(define (pred nnint) (add1 nnint))

;; need an accumulator for tracking the depth into the list and multiplication of bases.
;; q + (N ** depth) and so on and so on
;; can use map and fold to apply helper function to each number, then sum them all.
(define (nnint2dec nnint N)
    (if (null? nnint)
        0
        (+ (car nnint) (nnint2dec (cdr nnint N)))))
