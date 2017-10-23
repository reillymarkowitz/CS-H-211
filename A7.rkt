#lang racket
(require test-engine/racket-tests)

;; Part 1: Map

; add1-all : List Int -> List Int
(define (add1-all ls) ;; You Complete
  (map add1 ls))

(check-expect (add1-all '()) '())
(check-expect (add1-all '(1 2 3 4 5)) '(2 3 4 5 6))

; maybe-sub-10 : List Int -> List Int
(define (maybe-sub-10 ls) ;; You Complete
  (map (λ (n) (if (>= n 10) (- n 10) n)) ls))

(check-expect (maybe-sub-10 '()) '())
(check-expect (maybe-sub-10 '(1 2 3)) '(1 2 3))
(check-expect (maybe-sub-10 '(10 11 12)) '(0 1 2))
(check-expect (maybe-sub-10 '(10 11 1 2 10 11)) '(0 1 1 2 0 1))

; swapper : List A, A, A -> List A
(define (swapper ls x y) ;; You Complete
  (map (λ (n)
         (cond
           [(equal? n x) y]
           [(equal? n y) x]
           [else n]))ls))

(check-expect (swapper '() 1 2) '())
(check-expect (swapper '(1 2 3 2 1) 1 2) '(2 1 3 1 2))
(check-expect (swapper '(1 0 1 0 1 1 0 0) 1 0) '(0 1 0 1 0 0 1 1))

(define-struct item (name price))

; items->names : List Item -> List Name
(define (items->names ls) ;; You Complete
  (map item-name ls))

(check-expect (items->names '()) '())
(check-expect (items->names (list (item "milk" 3.00)
                                  (item "bread" 2.00)
                                  (item "cheese" 4.00)))
              '("milk" "bread" "cheese"))

;; Part 2: Let

; pow : Int , Int -> Int
(define (pow x n) ;; You Complete
  (cond
    [(equal? n 0) 1]
    [(even? n) (let ([z (pow x (/ n 2))]) (* z z))]
    [else (* x (pow x (sub1 n)))]))

(check-expect (pow 2 3) 8)
(check-expect (pow 2 5) 32)

;; Part 3: Folds that return Lists

; append : List , List -> List
(define (append ls1 ls2) ;; You Complete
  (foldr cons ls2 ls1))

(check-expect (append '() '()) '())
(check-expect (append '() '(1 2 3 4)) '(1 2 3 4))
(check-expect (append '(1 2 3 4) '()) '(1 2 3 4))
(check-expect (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; replace : List Int , Int , Int -> List Int
(define (replace ls old new) ;; You Complete
  (foldr (λ (n x)
           (cond
             [(equal? n old) (cons new (replace (rest ls) old new))]
             [else (cons n (replace (rest ls) old new))])) null ls))

(check-expect (replace '(1 2 3 4 5) 3 4) '(1 2 4 4 5))
(check-expect (replace '(1 2 4) 3 4) '(1 2 4))
(check-expect (replace '() 1 2) '())

; insert-at-end: List , Int -> List
(define (insert-at-end ns n) ;; You Complete
  (foldr cons (list n) ns))

(check-expect (insert-at-end '(1 2 3 4) 5) '(1 2 3 4 5))
(check-expect (insert-at-end '() 1) '(1))

; concat : List (List) -> List
(define (concat lss) ;; You Complete
  (foldr append '() lss))

(check-expect (concat '()) '())
(check-expect (concat '(() () ())) '())
(check-expect (concat '((1 2 3) (4 5 6) (7 8 9))) '(1 2 3 4 5 6 7 8 9))
(check-expect (concat '(((1)) ((2)) ((3)))) '((1) (2) (3)))
(check-expect (concat (concat '(((1)) ((2)) ((3))))) '(1 2 3))

;; Part 4: Folds that return other things

; sum : List Int -> Int
(define (sum ls) ;; You Complete
  (foldr + 0 ls))

(check-expect (sum '()) 0)
(check-expect (sum '(1 2 3)) 6)
(check-expect (sum '(2 4 6 8 10)) 30)

; count-occurrences : List X , X -> Int
(define (count-occurrences ls val) ;; You Complete
  (foldr + 0 (map (λ (x) (cond [(equal? x val) 1] [else 0])) ls)))

(check-expect (count-occurrences '() 2) 0)
(check-expect (count-occurrences '(1 2 3 2 3) 5) 0)
(check-expect (count-occurrences '(1 2 3 2 3) 2) 2)
(check-expect (count-occurrences (string->list "android andy ant nat late tell tale") #\a) 6)

; all-even: List Int -> Bool
(define (all-even? ls) ;; You Complete
  (foldr (λ (x y) (even? x)) #t ls))

(check-expect (all-even? '()) #t)
(check-expect (all-even? '(1 2 3)) #f)
(check-expect (all-even? '(2 4 6)) #t)

; any-even: List Int -> Bool
(define (any-even? ls) ;; You Complete
   (foldr (λ (x y) (or (even? x) y)) #f ls))

(check-expect (any-even? '()) #f)
(check-expect (any-even? '(1 2 3)) #t)
(check-expect (any-even? '(1 3 5)) #f)

;; Part 5: Defining your own abstractions

; andmap : (X -> Bool), List X -> Bool
(define (my-andmap f ls) ;; You Complete
  (foldr (λ (x y) (and x y)) #t (map f ls)))

(check-expect (my-andmap even? '()) #t)
(check-expect (my-andmap even? '(1 2 3)) #f)
(check-expect (my-andmap even? '(2 4 6)) #t)

; ormap : (X -> Bool), List X -> Bool
(define (my-ormap f ls) ;; You Complete
  (foldr (λ (x y) (or x y)) #f (map f ls)))

(check-expect (my-ormap even? '()) #f)
(check-expect (my-ormap even? '(1 2 3)) #t)
(check-expect (my-ormap even? '(1 3 5)) #f)

(test)