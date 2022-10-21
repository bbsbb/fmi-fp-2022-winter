#lang racket

;; On Lambda & Higher Order Functions
(define (moi-aussi-lambda n) (+ 1 n))
;; Those two are the same - fn above, symbol below.
(define je-suis-lambda (lambda (n) (+ 1 n)))


;;(je-suis-lambda 5)

(define (compose-two f1 f2)
  (lambda (x) (f2 (f1 x))))


(define increment-by-two (compose-two je-suis-lambda moi-aussi-lambda))

;;(increment-by-two 5)

;;((compose-two je-suis-lambda (lambda (x) (+ 5 x))) 2)

;; Returns proc!
;;(compose-two je-suis-lambda (lambda (x) (+ 5 x)))

;; Automorphic from wiki
(define (automorphic? n)
  (define (is-suffix? pow current)
    (cond
      [(= 0 current) #t]
      [(not (= (remainder pow 10) (remainder current 10))) #f]
      [else (is-suffix? (quotient pow 10) (quotient current 10))]))
  (is-suffix? (* n n) n))



;; Traversing numbers w/ recursion
;;(remainder 25 10)
;;(quotient 25 10)


(define (narcissistic? n)
  (define (number-length n)
    (cond
      [(= 0 n) 0]
      [else (+ 1 (number-length (quotient n 10)))]))
  (define (powered-digits digits pow)
    (cond
     [(= 0 digits) 0]
     [else (+ (expt (remainder digits 10) pow)
              (powered-digits (quotient digits 10) pow))]))
  (= n (powered-digits n (number-length n))))

;;(narcissistic? 153)
