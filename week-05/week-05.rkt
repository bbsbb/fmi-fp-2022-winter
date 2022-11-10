#lang racket

(define (is-prime-helper? n current)
  (cond
    [(= current n) #t]
    [(= 0 (remainder n current)) #f]
    [else (is-prime-helper? n (+ current 1))]))

(define (is-prime? n)
  (is-prime-helper? n 2))


(define (my-map xs fn)
  (cond
   [(null? xs) '()]
   [else (cons (fn (car xs))
               (my-map (cdr xs) fn))]))

;; Task 1: for each element in a list of integers - replace it with a tuple of:
;; - the nearest prime lower than the number
;; - the nearest prime higher than the number
;; e.g. '(8, 24) -> '((7 . 11) (23 . 29))

(define (low-prime n)
  (cond
    [(is-prime? n) n]
    [else (low-prime (- n 1))]))

(define (high-prime n)
  (cond
    [(is-prime? n) n]
    [else (high-prime (+ n 1))]))

(define (low-and-high-prime n)
  (cons (low-prime (- n 1)) (high-prime (+ n 1))))

(define (low-and-high-prime-of-list xs)
  (my-map xs low-and-high-prime))

;; (low-and-high-prime-of-list '(8 24))


;; Task 2: Same as task 1, but the numbers in the result should be unique
;; e.g. (low-high-prime-unique-list '(8 12)) -> '((7 . 11) (5 . 13))
;;                                                          ^ 7 is the nearest prime to 12, but it was already present in the first tuple

(define (contains? n list-of-pairs)
  (cond
    [(null? list-of-pairs) #f]
    [(or (= n (caar list-of-pairs))
         (= n (cdar list-of-pairs))) #t]
    [else (contains? n (cdr list-of-pairs))]))

(define (low-prime-unique n used)
  (cond
    [(and (is-prime? n) (not (contains? n used))) n]
    [else (low-prime-unique (- n 1) used)]))

(define (high-prime-unique n used)
  (cond
    [(and (is-prime? n) (not (contains? n used))) n]
    [else (high-prime-unique (+ n 1) used)]))

(define (low-high-prime-unique n used)
  (cons (low-prime-unique (- n 1) used)
        (high-prime-unique (+ n 1) used)))

(define (low-high-prime-unique-list xs)
  (define (iter as acc)
    (cond
      [(null? as) acc]
      [else (iter (cdr as) (cons (low-high-prime-unique (car as) acc)
                                 acc))]))
  (reverse (iter xs null)))

;; (low-high-prime-unique-list '(8 12))


;; Task 3: Histogram of vowels and consonants in a list of strings
;; e.g. (histogram-vowels (list "abc" "def")) -> '(2 . 4) ;; 2 vowels and 4 consonants
(define (vowel? letter)
  (member letter (string->list "aAyYoOuUeEiI")))

(define (flatten xs)
  (cond
    [(null? xs) null]
    [else (append (string->list (car xs))
                  (flatten (cdr xs)))]))

(define (histogram-vowels xs)
  (define (iter as acc)
    (cond
      [(null? as) acc]
      [(vowel? (car as)) (iter (cdr as)
                               (cons (+ 1 (car acc))
                                     (cdr acc)))]
      [else (iter (cdr as)
                  (cons (car acc)
                        (+ 1 (cdr acc))))]))
  (iter (flatten xs) (cons 0 0)))


;; (histogram-vowels (list "abc" "def"))

;; Task 4: Histogram of characters
;; e.g. (histogram (list "abcdaaxd" "xx")) -> '((#\x . 3) (#\d . 2) (#\c . 1) (#\b . 1) (#\a . 3))
(define (count-occurances xs x)
  (cond
    [(null? xs) 0]
    [(eq? x (car xs)) (+ 1 (count-occurances (cdr xs) x))]
    [else (count-occurances (cdr xs) x)]))

(define (histogram xs)
  (define (iter as acc)
    (cond
      [(null? as) acc]
      [(member (car as)
                (my-map acc car)) (iter (cdr as)
                                        acc)]
      [else (iter (cdr as)
                  (cons (cons (car as)
                              (count-occurances as (car as)))
                        acc))]))
  (iter (flatten xs) null))

;; (histogram (list "abcdaaxd" "xx"))


;; Task 5: Given a 2D matrix, return if the matrix is a square
;; e.g. (is-square? (list (list 1 2)
;;                        (list 3 4))) -> #t
(define (for-all? xs f)
  (cond
    [(null? xs) #t]
    [(f (car xs)) (for-all? (cdr xs) f)]
    [else #f]))

(define (is-square? mat)
  (for-all mat (lambda (row) (= (length row)
                                (length mat)))))
(is-square? (list (list 1 2) (list 3 4)))


;; Task 6: Given a matrix and a number, return all coordinates of the occurances of the number in the matrix
;; e.g.
;; (find-occurances-positions-2d (list (list 1 2 3)
;;                                     (list 3 4 5)
;;                                     (list 3 3 2))
;;                                3) -> '((0 . 2) (1 . 0) (2 . 0) (2 . 1))
(define (find-occurances-positions xs x)
  (define (iter as index-offset)
    (cond
      [(null? as) null]
      [(eq? (car as) x) (cons index-offset
                              (iter (cdr as) (+ 1 index-offset)))]
      [else (iter (cdr as) (+ 1 index-offset))]))
  (iter xs 0))


(define (find-occurances-positions-2d mat x)
  (define (iter rows index-offset)
    (cond
      [(null? rows) null]
      [else (append (my-map (find-occurances-positions (car rows) x)
                    (lambda (elem) (cons index-offset elem)))
            (iter (cdr rows) (+ 1 index-offset)))]
    ))
  (iter mat 0))

;;(find-occurances-positions-2d (list (list 1 2 3)
;;                                    (list 3 4 5)
;;                                    (list 3 3 2))
;;                               3)

