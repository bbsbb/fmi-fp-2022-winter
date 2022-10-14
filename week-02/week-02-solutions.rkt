#lang racket

(define (fact n)
  (cond
    [(> 2 n) 1]
    [else (* n (fact (- n 1)))]))
;;(fact 5)


(define (ascending-three n m poop)
  (cond
    [(and (< n m) (< m poop)) #t]
    [else #f]))

(define (asc-order n m poop)
  (< n m poop))

;;(asc-order 3 5 7)
;;(asc-order 5 6 5)


(define (reverse-digits-acc n)
  (define (reverse-acc n acc)
    (cond
      [(= 0 n) acc]
      [else (reverse-acc (quotient n 10)
                         (+ (* acc 10) (remainder n 10)))]))
  (reverse-acc n 0))

;; Can also be a helper inside.
(define (is-prime-helper? n current)
  (cond
    [(= current n) #t]
    [(= 0 (remainder n current)) #f]
    [else (is-prime-helper? (+ current 1))]))

(define (is-prime? n)
  (is-prime-helper? n 2))


(define (ascending-digits? n)
  (cond
    [(< n 10) #t]
    [(< (remainder n 10) (remainder (quotient n 10) 10)) #f]
    [else (ascending-digits? (quotient n 10))]))

;;(ascending-digits? 1233333456) => #t
;;(ascending-digits? 1273333456) => #f

;; (define ascending-digits?
;;   (lambda (n)
;;     (cond
;;       [(< n 10) #t]
;;       [(< (remainder n 10) (remainder (quotient n 10) 10)) #f]
;;       [else (ascending-digits? (quotient n 10))])))


(define (claim-adjuster percent)
  (lambda (amount)
    (+ amount (* (/ percent 100) amount))))

((claim-adjuster 10) 5)

;;((claim-adjuster 10) 1000) => 1100
;;(claim-adjust 1000 10)

(define minimal-pct-adjuster (claim-adjuster 5))
(define bad-debtor-adjuster (claim-adjuster 17))
