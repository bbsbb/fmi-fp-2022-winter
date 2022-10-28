#lang racket

;; Chat about tuples - what are they? Are they useful?

(let* ([t '(5 . 2)]
       [t-proper (cons 7 3)]
       [a-list (list 5 2 4 5 )])
  ;;(append '(5 2) '(5))
  (empty? '())
                                        ; ; (cons a-list 5)
  ;;(reverse (cons 5 (reverse a-list)))
  ;;(list? (cons 5 a-list))
  ;;(list t t-proper)
  ;;(cadddr a-list)
  ;;(cons 5)
;;  (list? a-list)
  )

;;(eq? '() null)
;;(null? '())

(define (ze-length xs)
  (cond
   [(null? xs) 0]
   [else (+ 1 (ze-length (cdr xs)))]))


;;(ze-length '( 5 5 5 5 5))

(define (occurences xs x)
  (cond
   [(null? xs) 0]
   [(eq? x (car xs)) (+ 1 (occurences (cdr xs) x))]
   [else (occurences (cdr xs) x)]))

(let ([a (box 5)])
  (set-box! a 7)
  a)




;;(member 2 '(5 3 2))

(define (dedup xs)
  (define (dedup-with-acc current accumulator)
    (cond
     [(null? current) accumulator]
     [(member (car current) accumulator) (dedup-with-acc (cdr current) accumulator)]
     [else (dedup-with-acc (cdr current) (cons (car current) accumulator))]))
  (reverse (dedup-with-acc xs '())))

;;(dedup '(5 5 3 5 5 3 2))


(define (my-map xs fn)
  (cond
   [(null? xs) '()]
   [else (cons (fn (car xs))
               (my-map (cdr xs) fn))]))

;;(my-map '(5 4 3) (lambda (n) (+ 1 n)))



;;(dedup '(5 5 3 2 1 2)) => '(5 3 2 1)
;; '(5 3 1 2)


;;(cons 5 (cons 2 (cons 3 '())))








;; What is a list?
































;; Ex 1: Write length.
