#lang racket
(require test-engine/racket-tests)

;; Part 1: Warm-Up

; count-to-1 : Int -> List Int
(define (count-to-1 n) ;; You Complete
  (build-list n (λ (x) (- n x))))

(check-expect (count-to-1 0) '())
(check-expect (count-to-1 10) '(10 9 8 7 6 5 4 3 2 1))
(check-expect (count-to-1 5) '(5 4 3 2 1))

; to-set : List X -> List X
(define (to-set ls) ;; You Complete
  (foldr (λ(x acc) (if (equal? (member x acc) #f) (cons x acc) acc)) '() ls))

(check-expect (to-set '()) '())
(check-expect (to-set '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (to-set '(1 2 3 2 4 2 5 1)) '(3 4 2 5 1))

; replicate-3-ls : X -> List X
(define (replicate-3-ls x) ;; You Complete
  (list x x x))

(check-expect (replicate-3-ls 5) (list 5 5 5))
(check-expect (replicate-3-ls (list 1 2 3))
              (list (list 1 2 3) (list 1 2 3) (list 1 2 3)))

;; Part 2: Key-Value Pairs

; key-value struct
; Key-Value K V
(define-struct kv
               (key value) #:transparent)

; map-countdown : Key-Value K Int -> List (Key-Value K Int)
(define (map-countdown x) ;; You Complete
  (map (λ (n) (kv (kv-key x) n)) (count-to-1 (kv-value x))))

(check-expect (map-countdown (kv "abc" 0)) '())
(check-expect (map-countdown (kv "abc" 3))
              (list (kv "abc" 3)
                    (kv "abc" 2)
                    (kv "abc" 1)))

; map-per-key : (KeyValue K V -> List (KeyValue K2 V2))
;               , List (Key-Value K V)
;               -> List (KeyValue K2 V2)
(define (map-per-key f keys) ;; You Complete
  (foldr (λ(e acc) (append (f e) acc)) '() keys))

(check-expect (map-per-key (λ (x) (list x)) '())   '())

(check-expect
 (map-per-key (lambda (x) (list (kv (kv-key x) (+ 5 (kv-value x)))))
              (list (kv "a" 10)))
 (list (kv "a" 15)))

(check-expect
 (map-per-key replicate-3-ls (list (kv "a" 5) (kv "b" 7)))
 (append (replicate-3-ls (kv "a" 5)) (replicate-3-ls (kv "b" 7))))

(check-expect
 (map-per-key replicate-3-ls (list (kv "a" 5) (kv "b" 7) (kv "c" 8)))
 (append (replicate-3-ls (kv "a" 5))
         (append (replicate-3-ls (kv "b" 7))
                 (replicate-3-ls (kv "c" 8)))))

;; Part 3: Dictionaries

(define-struct dict-entry
  (key values) #:transparent)

(define dict-entry-a (dict-entry "a" '(6)))
(define dict-entry-b (dict-entry "b" '(1 2)))
(define dict-entry-c (dict-entry "c" '(2 5 3)))
(define dict-entry-d (dict-entry "d" '(20 10)))

(define dict-0
  (list dict-entry-b dict-entry-c dict-entry-d))

(define dict-1 (list dict-entry-a dict-entry-b dict-entry-c dict-entry-d))

; Dict K V = List (Dict-Entry K (List V))
; insert-value : Dict-Entry K (List V) , V -> Dict K V
(define (insert-value tentry val) ;; You Complete
  (dict-entry (dict-entry-key tentry) (append (list val) (dict-entry-values tentry))))

(check-expect
 (insert-value (dict-entry "a" '()) 5)
 (dict-entry "a" '(5)))

(check-expect
 (insert-value (dict-entry "a" '(5)) 10)
 (dict-entry "a" '(10 5)))

(check-expect
 (insert-value (dict-entry "a" '(10 5)) 2)
 (dict-entry "a" '(2 10 5)))

 ;Dict K V = List (Dict-Entry K (List V))
 ;insert-into-group : Dict K V , Key-Value K V -> Dict K V
(define (insert-into-group x dict) ;; You Complete
  (define (helper bs pos)
    (cond
      [(empty? bs) -1]
      [(first bs) pos]
      [(helper (rest bs) (add1 pos))]))
  (define i (helper (map (λ(n) (equal? (kv-key x) (dict-entry-key n))) dict) 0))
  (if (>= i 0)
      ((lambda(b) (list-update dict i (λ(n) b))) (insert-value (list-ref dict i) (kv-value x)))
      (append dict (list (dict-entry (kv-key x) (list (kv-value x)))))))

(check-expect (insert-into-group (kv "a" 6) '())
              (list (dict-entry "a" '(6))))

(check-expect (insert-into-group (kv "a" 6) dict-0)
              (list dict-entry-b dict-entry-c dict-entry-d dict-entry-a))

(check-expect (insert-into-group (kv "a" 8) dict-1)
              (list (dict-entry "a" '(8 6)) dict-entry-b dict-entry-c dict-entry-d))

; group-by-key : List (Key-Value K V) -> Dict K V
(define (group-by-key ls) ;; You Complete
  (foldr insert-into-group '() ls))

(check-expect (group-by-key '()) '())
(check-expect (group-by-key (list (kv "a" 20))) (list (dict-entry "a" '(20))))
(check-expect (group-by-key (list (kv "a" 20) (kv "a" 10))) (list (dict-entry "a" '(20 10))))

(check-expect
  (group-by-key (list (kv "d" 20) (kv "d" 10)
                      (kv "c" 2)  (kv "c" 5) (kv "c" 3)
                      (kv "b" 1)  (kv "b" 2)
                      (kv "a" 6)))
  dict-1)

(check-expect 
 (group-by-key (map-per-key (lambda (x) (map (lambda (y) (kv (kv-key x) y)) (count-to-1 (kv-value x))))
                            (list (kv "a" 10) (kv "b" 5))))
 (list (dict-entry "b" '(5 4 3 2 1))
       (dict-entry "a" '(10 9 8 7 6 5 4 3 2 1))))

;; Part 4: Reduce

; sum-reduce : Dict-Entry K (List Int) -> Key-Value K Int
(define (sum-reduce x) ;; You Complete
  (kv (dict-entry-key x) (foldr + 0 (dict-entry-values x))))

(check-expect (sum-reduce (dict-entry "text" '()))      (kv "text" 0))
(check-expect (sum-reduce (dict-entry "a" '(1 2 3)))    (kv "a" 6))
(check-expect (sum-reduce (dict-entry 101 '(1 2 3 4)))  (kv 101 10))

; prod-reduce : Dict-Entry K (List Int) -> Key-Value K Int
(define (prod-reduce x) ;; You Complete
  (kv (dict-entry-key x) (foldr * 1 (dict-entry-values x))))

(check-expect (prod-reduce (dict-entry "text" '()))      (kv "text" 1))
(check-expect (prod-reduce (dict-entry "a" '(4 3 2 1)))  (kv "a" 24))
(check-expect (prod-reduce (dict-entry 5 '(5 4 3 2 1)))  (kv 5 120))

; reduce-per-key : (Dict-Entry K (List V) -> Key-Value K V) , Dict K V -> List (Key-Value K V)
(define (reduce-per-key f dict) ;; You Complete
  (foldr (λ (e acc1) ((λ(x acc2) (append (list x) acc2)) (f e) acc1)) '() dict))

(check-expect (reduce-per-key sum-reduce '())
              '())

(check-expect (reduce-per-key sum-reduce dict-1)
              (list (kv "a" 6) (kv "b" 3) (kv "c" 10) (kv "d" 30)))

(check-expect (reduce-per-key prod-reduce dict-1)
              (list (kv "a" 6) (kv "b" 2) (kv "c" 30) (kv "d" 200)))

(check-expect 
 (reduce-per-key prod-reduce
                 (list (dict-entry "a" '(5 4 3 2 1))
                       (dict-entry "b" '(3 2 1))
                       (dict-entry "c" '(1 3 2 6 8 15 17))))
 (list (kv "a" 120) (kv "b" 6) (kv "c" 73440)))

;; Part 5: Map Reduce

; map-reduce : (Key-Value K V -> List (Key-Value K2 V2)
;              ,  (Dict-Entry K2 (List V2) -> Key-Value K3 V3)
;              ,  List (Key-Value K V)
;              -> List (Key-Value K3 V3)
(define (map-reduce mapf reducef ls) ;; You Complete
  (reduce-per-key reducef (group-by-key (map-per-key mapf ls))))

(check-expect (map-reduce replicate-3-ls prod-reduce (list (kv "a" 5) (kv "b" 3) (kv "c" 10)))
              (list (kv "c" 1000) (kv "b" 27) (kv "a" 125)))

(check-expect (map-reduce map-countdown prod-reduce (list (kv "a" 5) (kv "b" 3) (kv "c" 10)))
              (list (kv "c" 3628800) (kv "b" 6) (kv "a" 120)))

;; Final Part

; The first check-expect takes a list of keys, and returns a new list of keys where the values have been cubed.
; The second check-expect takes a list of keys, and returns a new list of keys where each value n has been replaced with n!

(test)