#lang racket
(require test-engine/racket-tests)

;; Part I: map-reduce

; key-value and dict-entry
(define-struct kv (key value) #:transparent)
(define-struct dict-entry (key values) #:transparent)

(define (lookup-by-key k ls)
  (cond
    [(empty? ls) #f]
    [(equal? k (kv-key (first ls))) (kv-value (first ls))]
    [else (lookup-by-key k (rest ls))]))

; map-per-key : (KeyValue K V -> List (KeyValue K2 V2))
; , List (Key-Value K V)
; -> List (KeyValue K2 V2)
(define (map-per-key f keys)
  (foldr append '() (map f keys)))

; group-by-key : List (Key-Value K V) -> Dict K V
(define (group-by-key ls)
  (define (insert-value entry val)
    (dict-entry (dict-entry-key entry)
                (cons val (dict-entry-values entry))))
  (define (insert-into-group x dict)
    (if (empty? dict)
        (list (dict-entry (kv-key x) (list (kv-value x))))
        (let ([entry (first dict)])
          (if (equal? (kv-key x) (dict-entry-key entry))
              (cons (insert-value entry (kv-value x)) (rest dict))
              (cons entry (insert-into-group x (rest dict)))))))
  (foldr insert-into-group '() ls))

; reduce-per-key : (Dict-Entry K (List V) -> Key-Value K V) , Dict K V -> List (Key-Value K V)
(define (reduce-per-key f dict)
  (map f dict))

; map-reduce : (Key-Value K V -> List (Key-Value K2 V2)
; , (Dict-Entry K2 (List V2) -> Key-Value K3 V3)
; , List (Key-Value K V)
; -> List (Key-Value K3 V3)
(define (map-reduce mapf reducef ls)
  (reduce-per-key reducef (group-by-key (map-per-key mapf ls))))

(define (swap x) ; (kv 1 '("a" "b" "c" "q")) -> (list (kv "a" 1) (kv "b" 1) (kv "c" 1) (kv "q" 1))
  (foldr (lambda (m n) (cons (kv m (kv-key x)) n)) '() (kv-value x)))

(define (make-index ls) ;; You Complete
  (map-reduce swap (λ(x) (kv (dict-entry-key x) (dict-entry-values x))) ls))

(check-expect (make-index (list (kv 1 '("a" "b" "c" "q")) (kv 2 '("b" "c" "d")) (kv 3 '("q" "r" "b"))))
              (list (kv "b" '(1 2 3)) (kv "r" '(3)) (kv "q" '(1 3)) (kv "d" '(2)) (kv "c" '(1 2)) (kv "a" '(1))))

(check-expect
  (lookup-by-key "dog"
    (make-index (list (kv 1 '("cat" "dog" "bat" "lizard"))
                      (kv 2 '("dog" "bat" "snail"))
                      (kv 3 '("lizard" "rabbit" "bat")))))
  '(1 2))

(define (make-frequency-list ls) ;; You Complete
  (map-reduce swap (λ(x) (kv (dict-entry-key x) (length (dict-entry-values x)))) ls))

(check-expect
  (make-frequency-list
    (list (kv 1 '("cat" "dog" "bat" "lizard"))
          (kv 2 '("dog" "bat" "snail" "dog"))
          (kv 3 '("lizard" "rabbit" "bat"))))
 (list (kv "bat" 3) (kv "rabbit" 1) (kv "lizard" 2)
       (kv "dog" 3) (kv "snail" 1) (kv "cat" 1)))

(check-expect
 (lookup-by-key "dog"
   (make-frequency-list
     (list (kv 1 '("cat" "dog" "bat" "lizard" "dog"))
           (kv 2 '("dog" "bat" "snail"))
           (kv 3 '("lizard" "rabbit" "bat")))))
  3)

;; Part II: Trees

; tree structs and constructors
(define-struct empty-tree () #:transparent)
(define-struct tree (value left right) #:transparent)

(define (make-leaf v) (tree v (empty-tree) (empty-tree)))

; example trees
(define tree-0 (empty-tree))
(define tree-1 (make-leaf 2))
(define tree-2 (tree 5 tree-1 (make-leaf 7)))
(define tree-3 (tree 15 (make-leaf 11) (make-leaf 18)))
(define tree-4 (tree 9 tree-2 tree-3))
(define tree-5 (tree 5 tree-0 (tree 6 tree-0 (tree 7 tree-0 (make-leaf 8)))))
(define tree-6 (tree 5 (tree 4 (make-leaf 3) tree-0) tree-0))
(define tree-7 (tree 10 (tree 5 tree-0 (make-leaf 7)) (tree 15 (make-leaf 12) tree-0)))

; helper for drawing trees
(require pict/tree-layout)
(require pict)

(define (draw-tree tr)
  (define (node-img value)
    (cc-superimpose (disk 20 #:color "White" #:border-color "Black")
                    (text (number->string value))))
  (define (helper tr)
    (cond
      [(empty-tree? tr) (tree-layout #:pict (text "mt") #f #f)]
      [else (tree-layout
              #:pict (node-img (tree-value tr))
             (tree-edge (helper (tree-left tr)))
             (tree-edge (helper (tree-right tr))))]))
  (binary-tidier (helper tr)))

(displayln "tree-7: ")
(draw-tree tree-7)

; tree-size : Tree -> Int
(define (tree-size tree)
  (if (empty-tree? tree)
      0
      (+ 1 (tree-size (tree-left tree)) (tree-size (tree-right tree)))))

(check-expect (tree-size tree-0) 0)
(check-expect (tree-size tree-1) 1)
(check-expect (tree-size tree-2) 3)
(check-expect (tree-size tree-3) 3)
(check-expect (tree-size tree-4) 7)
(check-expect (tree-size tree-5) 4)
(check-expect (tree-size tree-6) 3)
(check-expect (tree-size tree-7) 5)

; tree-height : Tree -> Int
(define (tree-height tr)
  (if (empty-tree? tr)
      0
      (+ 1 (max (tree-height (tree-left tr)) (tree-height (tree-right tr))))))

(check-expect (tree-height tree-0) 0)
(check-expect (tree-height tree-1) 1)
(check-expect (tree-height tree-2) 2)
(check-expect (tree-height tree-3) 2)
(check-expect (tree-height tree-4) 3)
(check-expect (tree-height tree-5) 4)
(check-expect (tree-height tree-6) 3)
(check-expect (tree-height tree-7) 3)

; contains? : Tree  Int -> Bool
(define (contains? tr val)
  (cond
    [(empty-tree? tr) #f]
    [(equal? val (tree-value tr)) #t]
    [else (or (contains? (tree-left tr) val) (contains? (tree-right tr) val))]))

(check-expect (contains? tree-0 0) #f)
(check-expect (contains? tree-3 11) #t)
(check-expect (contains? tree-3 18) #t)
(check-expect (contains? tree-4 18) #t)
(check-expect (contains? tree-4 3) #f)
(check-expect (contains? tree-5 5) #t)
(check-expect (contains? (tree 10 (make-leaf 4) (make-leaf 5)) 5) #t)
(check-expect (contains? tree-5 3) #f)
(check-expect (contains? tree-7 7) #t)

; left-most : Tree -> Int
(define (left-most tr)
  (cond
    [(empty-tree? tr) #f]
    [(empty-tree? (tree-left tr)) (tree-value tr)]
    [else (left-most (tree-left tr))]))

(check-expect (left-most tree-0) #f)
(check-expect (left-most tree-3) 11)
(check-expect (left-most tree-4) 2)
(check-expect (left-most tree-5) 5)
(check-expect (left-most tree-6) 3)
(check-expect (left-most tree-7) 5)

; right-most : Tree -> Int
(define (right-most tr)
  (cond
    [(empty-tree? tr) #f]
    [(empty-tree? (tree-right tr)) (tree-value tr)]
    [else (right-most (tree-right tr))]))

(check-expect (right-most tree-0) #f)
(check-expect (right-most tree-1) 2)
(check-expect (right-most tree-2) 7)
(check-expect (right-most tree-3) 18)
(check-expect (right-most tree-5) 8)
(check-expect (right-most tree-6) 5)
(check-expect (right-most tree-7) 15)

; tree->list : Tree -> List
(define (tree->list tr)
  (cond
     [(empty-tree? tr) '()]
     [(number? tr) (list tr)]
     [else (append (tree->list (tree-left tr)) (tree->list (tree-value tr)) (tree->list (tree-right tr)))]))

(check-expect (tree->list tree-0) '())
(check-expect (tree->list tree-1) '(2))
(check-expect (tree->list tree-2) '(2 5 7))
(check-expect (tree->list tree-4) '(2 5 7 9 11 15 18))
(check-expect (tree->list tree-5) '(5 6 7 8))
(check-expect (tree->list tree-3) '(11 15 18))
(check-expect (tree->list tree-6) '(3 4 5))
(check-expect (tree->list tree-7) '(5 7 10 12 15))

; tree-mirror : Tree -> Tree
(define (tree-mirror tr)
  (if (empty-tree? tr)
      tr
      (make-tree (tree-value tr)
                 (tree-mirror (tree-right tr))
                 (tree-mirror (tree-left tr)))))

(check-expect (tree-mirror tree-0) (empty-tree))
(check-expect (tree-mirror tree-4)
 (tree 9
       (tree 15 (tree 18 (empty-tree) (empty-tree)) (tree 11 (empty-tree) (empty-tree)))
       (tree 5 (tree 7 (empty-tree) (empty-tree)) (tree 2 (empty-tree) (empty-tree)))))
(check-expect (tree-mirror tree-7)
 (tree 10
       (tree 15 (empty-tree) (tree 12 (empty-tree) (empty-tree)))
       (tree 5 (tree 7 (empty-tree) (empty-tree)) (empty-tree))))

; Algorithm:
;   given a tree tr and value val:
;   1. if tr is an empty tree, build a new node that contains val
;   2. if tr is a node with value v1 and val <= v1, insert val in the
;      left subtree of tr, and make a new tree node pointing to the old
;      right subtree and this new left subtree.
;   3. otherwise, insert val in the right subtree of tr, making
;      a new node that points to the new right subtree
;
; bst-add : Tree Int -> Tree
(define (bst-add tr val)
  (cond
    [(empty-tree? tr) (make-leaf val)]
    [(<= val (tree-value tr)) (tree (tree-value tr) (make-leaf val) (tree-right tr))]
    [else (tree (tree-value tr) (tree-left tr) (make-leaf val))]))

(check-expect (bst-add (empty-tree) 5)
              (make-leaf 5))

(check-expect (bst-add (tree 5 (empty-tree) (empty-tree)) 4)
              (tree 5 (make-leaf 4) (empty-tree)))

(check-expect (bst-add (bst-add (bst-add (empty-tree) 5) 4) 6)
              (tree 5 (make-leaf 4) (make-leaf 6)))

(check-expect (bst-add (bst-add (bst-add (empty-tree) 5) 6) 4)
              (tree 5 (make-leaf 4) (make-leaf 6)))

; Algorithm:
;  given a tree tr and a value val:
;  1. if tr is empty, return #false
;  2. if tr is a node whose value equals val, return #true
;  3. if tr has a value v1 and val < v1, recur on the left subtree
;  4. otherwise, recur on the right subtree
;
; bst-contains? : Tree Int -> Bool
(define (bst-contains? tr val)
  (cond
    [(empty-tree? tr) #f]
    [(equal? val (tree-value tr)) #t]
    [(< val (tree-value tr)) (bst-contains? (tree-left tr) val)]
    [else (bst-contains? (tree-right tr) val)]))

(check-expect (bst-contains? tree-0 5) #f)
(check-expect (bst-contains? tree-1 2) #t)
(check-expect (bst-contains? tree-2 8) #f)
(check-expect (bst-contains? tree-3 11) #t)
(check-expect (bst-contains? tree-3 12) #f)
(check-expect (bst-contains? tree-5 8) #t)
(check-expect (bst-contains? tree-5 2) #f)

(test)