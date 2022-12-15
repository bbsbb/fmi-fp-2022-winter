#lang racket


(apply + '(1 2 3 4))
;; (+ 1 2 3 4)

(foldr + 0 '(1 2 3 4))
;; (+ 1 (+ 2 (+ 3 (+ 4))))

(apply list '(1 2 3 4))

(foldr list '() '(1 2 3 4))


(foldr append '() (list (list 1 2) (list 3 4)))

(foldl append '() (list (list 1 2) (list 3 4)))

(append '(1) '(2))


(foldr (lambda (one two) (cons two one)) -10 '(1 2 3 4))

(foldl (lambda (one two) (cons two one)) -10 '(1 2 3 4))

(define (member? x xs)
  (list? (member x xs)))

(define (all-the-same? xs ys)
  (foldl (lambda (y res) (and res
                         (member? y xs))) #t ys))


(all-the-same? (list 4) (list 4))


(cons 0 '(1 2 3))

(cons '(1 2 3) 0)

(append '(1 2) '(2 3))


(define (alphabet-validator-factory alphabet)
  (lambda (phrase) (all-the-same? (string->list phrase)
                             (string->list alphabet))))

(define (triangular? xss)
  (andmap (lambda (row row-index)
            (all-the-same? '(0) (take row row-index)))
          xss
          (range (length xss))))


(triangular? (list (list 1 1 1)
                   (list 0 1 1)
                   (list 0 0 1)))
(range 5)

((alphabet-validator? "abc") "I am not in the alphabet") ;; #f
((alphabet-validator? "abcdat") "cat") ;;#t
