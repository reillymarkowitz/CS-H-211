#lang racket
(require test-engine/racket-tests)
(require racket/set)
(require graph)
(require racket/random)

(define page1 (cons 1 '("the" "cat" "sat" "on" "the" "mat")))
(define page2 (cons 2 '("the" "dog" "stood" "on" "the" "mat")))
(define page3 (cons 3 '("the" "cat" "stood" "while" "a" "dog" "sat")))

(define (get-num page) (first page))
(define (get-words page) (rest page))

(define www (list page1 page2 page3))

(define (process-word ht p w)
  (cond
    [(hash-has-key? ht w)
     (if (equal? (memq p (hash-ref ht w)) #f)
         (hash-set ht w (cons p (hash-ref ht w)))
         ht)]
    [else (hash-set ht w (list p))]))

(check-expect (process-word (hasheq) 3 "hello")
              '#hasheq(("hello" . (3))))
(check-expect (process-word (process-word (hasheq) 3 "hello") 4 "bye")
              '#hasheq(("hello" . (3)) ("bye" . (4))))
(check-expect (process-word (process-word (process-word (hasheq) 3 "hello") 4 "bye") 5 "hello")
              '#hasheq(("hello" . (5 3)) ("bye" . (4))))
(check-expect (process-word (process-word (process-word (process-word (hasheq) 3 "hello") 4 "bye") 5 "hello") 3 "hello")
              '#hasheq(("hello" . (5 3)) ("bye" . (4))))

(define (process-page ht wp)
  (foldr (λ (word acc)
           (process-word acc (get-num wp) word))
         ht
         (get-words wp)))

(check-expect (process-page (hasheq) page1)
              '#hasheq(("the" . (1)) ("mat" . (1)) ("cat" . (1)) ("sat" . (1)) ("on" . (1))))
(check-expect (process-page (hasheq) page2)
              '#hasheq(("the" . (2)) ("mat" . (2)) ("dog" . (2)) ("on" . (2)) ("stood" . (2))))
(check-expect (process-page (hasheq) page3)
              '#hasheq(("the" . (3))
                       ("cat" . (3))
                       ("sat" . (3))
                       ("dog" . (3))
                       ("stood" . (3))
                       ("a" . (3))
                       ("while" . (3))))

(define (build-index ht ws)
  (foldr (λ (wp acc)
           (process-page acc wp))
         (hasheq)
         ws))

(check-expect (build-index (hasheq) www)
              '#hasheq(("the" . (1 2 3))
                       ("mat" . (1 2))
                       ("cat" . (1 3))
                       ("sat" . (1 3))
                       ("dog" . (2 3))
                       ("on" . (1 2))
                       ("stood" . (2 3))
                       ("a" . (3))
                       ("while" . (3))))

(define (search-query words ind)
  (let ([i (map (λ(x) (hash-ref ind x)) words)])
    (foldr set-intersect (apply append i) i)))

(check-expect (search-query '("cat" "dog") (build-index (hasheq) www))
              '(3))

(define wwwgraph
  (unweighted-graph/adj
   '((1 2)
     (2 3 12)
     (3 4 8 12)
     (4 8)
     (5 1)
     (6 1 2)
     (7 2 12)
     (8 9 12 13 14 15)
     (9 16)
     (10 5 6 7)
     (11 10)
     (12 16)
     (13 16)
     (14 16)
     (15 16)
     (16 11))))

(define (random-surf-k gr st i)
  (if (zero? i)
      '()
      (cons st
            (random-surf-k gr (random-ref (get-neighbors gr st)) (sub1 i)))))

(define (random-surf-k-with-restart gr p st i)
  (cond
    [(zero? i) '()]
    [(< (random) (/ p 100))
     (cons st
           (random-surf-k-with-restart gr p (random-ref (get-vertices gr)) (sub1 i)))]
    [else
     (cons st
           (random-surf-k-with-restart gr p (random-ref (get-neighbors gr st)) (sub1 i)))]))

(define (collect-statistics gr p i)
  (sort
   (hash-map
    (foldr (λ(x acc)
             (if (hash-has-key? acc x)
                 (hash-set acc x (add1 (hash-ref acc x)))
                 (hash-set acc x 1)))
           (hasheq)
           (random-surf-k-with-restart gr p
                                       (random-ref (get-vertices gr))
                                       i))
    (λ(k v) (cons k (exact->inexact (* 100 (/ v i))))))
   (λ(a b) (> (cdr a) (cdr b)))))

(test)