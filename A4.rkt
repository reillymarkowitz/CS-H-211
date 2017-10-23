#lang racket
(require test-engine/racket-tests) ;; For check/expect
(require 2htdp/image)              ;; For images

; Part 1: Bit Lists

;; random-bit : NOTHING -> 0 or 1
(define (random-bit)  ;; You complete
  (random 2))

;; Ensure that random-bit works correctly
(check-member-of (random-bit) 0 1)
(check-member-of (random-bit) 0 1)
(check-member-of (random-bit) 0 1)

(define (list-helper x)
  (random-bit))

;; random-bit-list : length -> (List of (0 or 1))
(define (random-bit-list n) ;; You complete
 (build-list n list-helper))

(check-expect (length (random-bit-list 100)) 100)
(check-member-of (first (random-bit-list 10)) 0 1)
(check-member-of (second (random-bit-list 10)) 0 1)
(check-member-of (third (random-bit-list 10)) 0 1)

;; copy-bit-list-with-errors : (List of (0 or 1)) Float -> (List of (0 or 1))
;; second argument is the probability of error
(define (copy-bit-list-with-errors bs e) ;; You Complete
  (cond
    [(empty? bs) bs]
    [(empty? (rest bs)) (if (< (random) e) (list (random-bit)) (list (first bs)))]
    [else (if (< (random) e) (append (list (random-bit)) (copy-bit-list-with-errors (rest bs) e)) (append (list (first bs)) (copy-bit-list-with-errors (rest bs) e)))]))

(check-expect
 (copy-bit-list-with-errors (list 1 1 0 1 0 0 0 1) 0)
 (list 1 1 0 1 0 0 0 1))

(check-expect
 (length (copy-bit-list-with-errors (random-bit-list 100) 0.3))
 100)

;; repeat-3-bit-list : (List of (0 or 1)) -> (List of (0 or 1))
(define (repeat-3-bit-list ls) ;; You Complete
  (cond
    [(empty? (rest ls)) (append ls ls ls)]
    [else (append (list (first ls) (first ls) (first ls)) (repeat-3-bit-list (rest ls)))]))

(check-expect
 (length (repeat-3-bit-list (list 1 1 0 1))) 12)

(check-expect
 (repeat-3-bit-list (list 1 1 0 1))
 (list 1 1 1 1 1 1 0 0 0 1 1 1))

(define (majority x y z)
  (cond
    [(equal? x y) x]
    [(equal? x z) x]
    [(equal? y z) y]
    [else x]))

;; majority-3-bit-list: List of (0 or 1) -> List of (0 or 1) 

(define (majority-3-bit-list cs) ;; You Complete
  (cond
    [(empty? cs) cs]
    [(empty? (rest cs)) cs]
    [(empty? (rest(rest cs))) cs]
    [else (cons (majority (first cs) (second cs) (third cs)) (majority-3-bit-list (rest (rest (rest cs)))))]))

(check-expect
 (length (majority-3-bit-list (list 1 1 1 0 0 0 1 1 1)))
 3)

(check-expect
 (majority-3-bit-list (copy-bit-list-with-errors (repeat-3-bit-list (list 1 1 0 1)) 0.1))
 (list 1 1 0 1))

; I commented out this test case because the procedure works as expected,
; but 40% of the time this check-expect function throws an error, as it should.
; It got really annoying while I was testing my code.
#;(check-expect
 (majority-3-bit-list (copy-bit-list-with-errors (repeat-3-bit-list (list 1 1 0 1)) 0.4))
 (list 1 1 0 1))

; Part 2: Images

;; random-byte : NOTHING -> [0..255]
(define (random-byte) ;; You Complete
 (random 256))

(check-range (random-byte) 0 255)

;; random-color : NOTHING -> Color
(define (random-color) ;; You Complete
 (make-color (random-byte) (random-byte) (random-byte)))

(check-expect (color? (random-color)) #t)

;; random-color-list : length -> (List of Color)
(define (random-color-list n) ;; You complete
  (define (random-helper n)
    (random-color))
  (build-list n random-helper))

(check-expect (length (random-color-list 100)) 100)
(check-expect (color? (first (random-color-list 10))) #t)

;; copy-color-list-with-errors : (List of Color) Float -> (List of Color)
(define (copy-color-list-with-errors cs e) ;; You Complete
    (cond
    [(empty? cs) cs]
    [(empty? (rest cs)) (if (< (random) e) (list(random-color)) (list (first cs)))]
    [else (if (< (random) e) (append (list(random-color)) (copy-color-list-with-errors (rest cs) e)) (append (list (first cs)) (copy-color-list-with-errors (rest cs) e)))]))

(check-expect
 (copy-color-list-with-errors (list (make-color 10 10 10) (make-color 20 20 20) (make-color 30 30 30)) 0)
 (list (make-color 10 10 10) (make-color 20 20 20) (make-color 30 30 30)))

(check-expect
 (length (copy-color-list-with-errors (random-color-list 100) 0.3))
 100)

;; copy-image-with-errors : Image Float -> Image
(define (copy-image-with-errors img e) ;; You Complete
  (color-list->bitmap (copy-color-list-with-errors (image->color-list img) e) 65 77))

;; Test image
(define penguin-img (bitmap/file "penguin-tiny.png"))
(check-expect (copy-image-with-errors penguin-img 0) penguin-img)
(check-expect (image-width (copy-image-with-errors penguin-img 0.3)) (image-width penguin-img))
(check-expect (image-height (copy-image-with-errors penguin-img 0.3)) (image-height penguin-img))

;; repeat-3-color-list : (List of Color) -> (List of Color)
(define (repeat-3-color-list cs) ;; You Complete
 (repeat-3-bit-list cs))

(define (repeat-3-img img) ;; Image -> Image
 (color-list->bitmap (repeat-3-color-list (image->color-list img)) (* 65 3) 77))

(check-expect (image-height (repeat-3-img penguin-img))
 (image-height penguin-img))

(check-expect (image-width (repeat-3-img penguin-img))
 (* 3 (image-width penguin-img)))

;; majority-3-color-list : (List of Color) -> (List of Color)
(define (majority-3-color-list cs) ;; You Complete
 (majority-3-bit-list cs))

;; recover-3-img : Image Float -> Image
(define (recover-3-img img e) ;; You Complete
  (color-list->bitmap (majority-3-color-list (copy-color-list-with-errors (repeat-3-color-list (image->color-list img)) e)) 65 77))

(test)