#lang racket

;; I am real
(define (my-map fn xs)
  (cond
   [(null? xs) '()]
   [else (cons (fn (car xs))
               (my-map fn (cdr xs)))]))

;;(my-map add1 '(1 2 3))

;;(map add1 '(1 2 3)) ;; ;)

;;(map add1 '(1 2 3) '(2 3 4)) ;;o(

;;(map + '(1 2 3) '(2 3 4)) ;; ;)

;;(map + '(1 2 3) '(2 3)) ;; ;(

;;(map + '(1 2 3) '(2 3 "a")) ;; ;(


;; filter

;;(filter odd? '(1 2 3 4 5))

;;(filter identity '(#t #f 1 null '()))


;;

;;(map + '(1 2 3 4))
;;(map max '(1 2 3 4))

;;(+ 1 2 3 4 5)

;; Enter apply
;;(apply + '(1 2 3 4 5))

;;(apply max '(1 2 3 4 5))



(define add-5 (curry + 5))

(add-5 10)

(define increment-list (curry map add1))
;;(increment-list '(1 2 3))


;;(map and '(#t #t))


;;
;;(apply and '(#t #t #t))

;;We have
;;(andmap identity '(#t #t #t #f))
;;(ormap identity '(#t #t #t #f))


;; Let's write a function that returns true if all
;; elements in a list are equal
(define (all? xs)
  (andmap (curry equal? (car xs))) xs)


;; Let's create mapcat/flatmap
(define (mapcat fn xs)
  (apply append (map fn xs)))

;;(mapcat (curry range 1) '(5 10 15))

;; Check if matrix?
(define (matrix? xss)
  (all? (map length xss)))

;; (matrix? (list (list 0 1 1)
;;                (list 0 0 1)
;;                (list 0 0 0))) ;; # (list (list 0 0 0)
;;                                        ;;(list 1 0 0)
;;                                        ;;(list 1 1 0))

;; (matrix? (list (list 0 0 0)
;;                (list 0 0 )
;;                (list 0 0 0))) ;; #f

;; (mapcat identity '((list 1 2 3) (list 4 5 6))) ;; ->  ;(1 ... 6)

;; (map identity '((list 1 2 3) (list 4 5 6)))


;;(range 1 5)

;; And the transpose
(define (transpose matrix)
  (apply map list matrix))
