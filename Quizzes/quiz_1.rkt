#lang eopl
(require "eopl-extras.rkt")



;; quiz #1 on problem 2.1 from the book, implementing nnint2dec and dec2nnint for bigits.
;; Numbers are represented in base N (large N)
;; The reprsentation uses a list of numbers such that each number is in 0...N-1 (called bigits)
;; zero, iszero?, succ, pred, nnint2dec, dec2nnint plus sub factorial

;; An nnint is  either ...
;;     '()
;;     (cons r '(q))
;; where r is greater than or equal to zero and less than N,
;; and q is q * N where N is the base (big N)

(define N 100)

(define (zero) '() )

(define (iszero? nnint) (null? nnint))

(define (succ nnint)
  (cond
    [(iszero? nnint) '(1)]
    [(< car (sub1 N))
     (cons (add1 (car nnint)) (cdr nnint)) ]
    [else (cons 0 (succ (cdr nnint)))]))

(define (pred nnint)
  (cond
    [(iszero? nnint) (eopl:error "0 does not have a predecessor")]
    [(and (null? cdr) (= (car nnint) 1))
     (zero)]
    [(> (car nnint) 0)
     (cons (sub1 (car nnint)) (cdr nnint))]
    [else (cons (sub1 N) (pred (cdr nnint)))]))
    


;; need an accumulator for tracking the depth into the list and multiplication of bases.
;; q + (N ** depth) and so on and so on
;; can use map and fold to apply helper function to each number, then sum them all.virtua
(define (nnint2dec nnint)
  (define (helper lon pow)
    (if (null? lon)
        0
        (+ (* (car lon) (expt N pow))
           (helper (cdr lon) (add1 pow)))))
  (helper nnint 0))
  
;; (define (nnint2dec nnint N)
;;     (if (null? nnint)
;;         0
;;         (+ (car nnint) (nnint2dec (cdr nnint N)))))


;;(define (zero) '(0))

;; (define (iszero? nnint)
;;   (and 
;;     (= (car nnint) 0)
;;     (= (length nnint) 1)))

;;this won't work
#| (define (succ nnint N) 
     (cond
       [(< (car nnint) (sub1 N)) (add1 (car nnint))]
       [(= (car nnint) (sub1 N)) (succ (cdr nnint) N)]
    [(null? cdr) (cons 1 nnint)])) |#

;;(define (pred nnint) (add1 nnint))

;;shit brysen found
;; (for/list ([(i x) (in-indexed '(1 2 3 4))])
;;   (* x i))