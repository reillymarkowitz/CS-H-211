#lang racket

;; For check/expect
(require test-engine/racket-tests)

;; Part 1: List Procedures

(define (length xs) ;; You Complete
  (if (empty? xs) 0 (add1(length (rest xs)))))

(check-expect (length '()) 0)
(check-expect (length '(1 2 3 4 5)) 5)

(define (sum xs) ;; You Complete
  (if (empty? xs) 0 (+ (first xs) (sum (rest xs)))))

(check-expect (sum '(1 2 3)) 6)
(check-expect (sum '(1)) 1)
(check-expect (sum '()) 0)

(define (product xs) ;; You Complete
  (if (empty? xs) 1 (* (first xs) (product (rest xs)))))

(check-expect (product '(1 2 3 4)) 24)
(check-expect (product '(1)) 1)
(check-expect (product '()) 1)

(define (replace ns old new) ;; You Complete
  (if (< (length ns) 1) ns (if (equal? (first ns) old) (cons new (replace (rest ns) old new)) (cons (first ns) (replace (rest ns) old new)))))

(check-expect (replace '(1 2 3 4 5) 3 4) '(1 2 4 4 5))
(check-expect (replace '(1 2 4) 3 4) '(1 2 4))
(check-expect (replace '() 1 2) '())

(define (take ns n) ;; You Complete
  (if (or (zero? n) (null? ns)) null (cons (first ns) (take (rest ns) (- n 1)))))

(check-expect (take '(1 2 3 4 5) 3) '(1 2 3))
(check-expect (take '(1 2 3) 5) '(1 2 3))
(check-expect (take '(1 2 3) 0) '())
(check-expect (take '() 5) '())

; Insert at the end
(define (insert-at-end ns n) ;; You Complete
  (append ns (list n)))

(check-expect (insert-at-end '(1 2 3 4) 5) '(1 2 3 4 5))
(check-expect (insert-at-end '() 1) '(1))

(define (difference ns) ;; You Complete
  (define (helper ns) (if (null? (rest ns)) '() (append (list (- (first ns) (second ns))) (helper (rest ns)))))
  (if (null? ns) '() (cons 0 (helper ns))))

(check-expect (difference '(1 2 3 4 5)) '(0 -1 -1 -1 -1))
(check-expect (difference '(5 5 2 2 2)) '(0 0 3 0 0))
(check-expect (difference '()) '())
(check-expect (difference '(1)) '(0))

;; Part 2: Larger List Procedures

(define (replace-and-sum-bad xs old new)
 (insert-at-end (replace xs old new) (sum (replace xs old new))))

(define (replace-and-add-sum xs old new) ;; You Complete
  (define (helper xs old new sum)
  (if (empty? xs) (cons sum '())
       (cond
        [(equal? (first xs) old) (cons new (helper (rest xs) old new (+ sum new)))]
        [else (cons (first xs) (helper (rest xs) old new (+ sum (first xs))))])))
  (helper xs old new 0))

(check-expect (replace-and-add-sum '(1 1 1) 1 2) (replace-and-sum-bad '(1 1 1) 1 2))
(check-expect (replace-and-add-sum '(1 2 5) 1 2) (replace-and-sum-bad '(1 2 5) 1 2))
(check-expect (replace-and-add-sum '(1 2 3 2 1) 2 5) (replace-and-sum-bad '(1 2 3 2 1) 2 5))

(define (compute-prefix-product-bad xs n)
 (product (take xs n)))

(define (compute-prefix-product xs n) ;; You Complete
 (define (helper xs n prod)
  (cond
      [(zero? n) prod]
      [(null? xs) 1]
      [(< (length xs) n) (product xs)]
      [else (helper (rest xs) (- n 1) (* prod (first xs)))]))
 (helper xs n 1))

(check-expect (compute-prefix-product '(1 2 3 4 5) 4) (compute-prefix-product-bad '(1 2 3 4 5) 4))
(check-expect (compute-prefix-product '(1 2 3 4 5) 3) (compute-prefix-product-bad '(1 2 3 4 5) 3))
(check-expect (compute-prefix-product '(3 3) 3) (compute-prefix-product-bad '(3 3) 3))
(check-expect (compute-prefix-product '(1) 1) (compute-prefix-product-bad '(1) 1))
(check-expect (compute-prefix-product '() 1) (compute-prefix-product-bad '() 1))

(define (average-bad xs)
  (if (empty? xs) 0 (/ (sum xs) (length xs))))

(define (average xs) ;; You Complete
  (define (helper xs sum len) (if (empty? xs) (/ sum len) (helper (rest xs) (+ sum (first xs)) (+ len 1))))
  (if (empty? xs) 0 (helper xs 0 0)))

(check-expect (average '(1 1 1)) (average-bad '(1 1 1)))
(check-expect (average '(3 3 0)) (average-bad '(3 3 0)))
(check-expect (average '()) (average-bad '()))

;; Part 3: Checksums

(define (insert-checksum-bad ns)
 (insert-at-end ns (modulo (sum ns) 10)))

(define (insert-checksum ns) ;; You Complete
  (define (helper ns sum) (if (empty? ns) (list (modulo sum 10)) (cons (first ns) (helper (rest ns) (+ sum (first ns))))))
  (helper ns 0))

(check-expect (insert-checksum '(1 2 3 4 5)) (insert-checksum-bad '(1 2 3 4 5)))
(check-expect (insert-checksum '()) '(0))
(check-expect (insert-checksum '(1 2 2 3 3 3 3 5)) (insert-checksum-bad '(1 2 2 3 3 3 3 5)))

(define (staircase-checksum-bad ns)
 (define (helper ns step)
   (if (empty? ns)
       0
       (+ (* (first ns) step) (helper (rest ns) (add1 step)))))
 (insert-at-end ns (modulo (helper ns 1) 10)))

(define (staircase-checksum ns) ;; You Complete
  (define (helper ns sum pos) (if (empty? ns) (list (modulo sum 10)) (cons (first ns) (helper (rest ns) (+ sum (* pos (first ns))) (add1 pos)))))
  (helper ns 0 1))

(check-expect (staircase-checksum '(1 2 3 5)) (staircase-checksum-bad '(1 2 3 5)))
(check-expect (staircase-checksum '()) (staircase-checksum-bad '()))
(check-expect (staircase-checksum '(1 2 2 3 3 3 3 5)) (staircase-checksum-bad '(1 2 2 3 3 3 3 5)))

(define (insert-2-checksums ns) ;; You Complete
  (define (helper ns i-sum s-sum pos) (if (empty? ns) (cons (modulo i-sum 10) (list (modulo s-sum 10))) (cons (first ns) (helper (rest ns) (+ i-sum (first ns)) (+ s-sum (* pos (first ns))) (add1 pos)))))
  (helper ns 0 0 1))

(check-expect (insert-2-checksums '(1 2 3 5)) '(1 2 3 5 1 4))
(check-expect (insert-2-checksums '()) '(0 0))
(check-expect (insert-2-checksums '(1 2 2 3 3 3 3 5)) '(1 2 2 3 3 3 3 5 2 7)) 

(test)