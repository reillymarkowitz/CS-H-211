#lang racket

(require test-engine/racket-tests)

(define-struct mcons (head [tail #:mutable]) #:transparent)

; make-mlist : number, number -> mcons
; Returns an mcons from i to j.
(define (make-mlist i j)
  (if (= i (add1 j))
      '()
      (mcons i (make-mlist (add1 i) j))))

(check-expect (make-mlist 2 5) (mcons 2 (mcons 3 (mcons 4 (mcons 5 '())))))
(check-expect (make-mlist 1 6) (mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 '())))))))
(check-expect (make-mlist 0 3) (mcons 0 (mcons 1 (mcons 2 (mcons 3 '())))))
(check-expect (make-mlist 10 12) (mcons 10 (mcons 11 (mcons 12 '()))))

; skip : number, mcons -> mcons
; Returns an mcons that is identical to xs, without the first n elements.
(define (skip n xs)
  (if (zero? n)
      xs
      (skip (sub1 n) (mcons-tail xs))))

(check-expect (skip 0 (make-mlist 1 6)) (make-mlist 1 6))
(check-expect (skip 1 (make-mlist 1 6)) (make-mlist 2 6))
(check-expect (skip 3 (make-mlist 1 6)) (make-mlist 4 6))

(define (mcons-length ls)
  (if (empty? (mcons-tail ls))
      1
      (add1 (mcons-length (mcons-tail ls)))))

(define (make-cyclic-mlist i j)
  (define (set-n-tail! xs newxs)
    (if (empty? (mcons-tail xs))
        (set-mcons-tail! xs newxs)
        (set-n-tail! (mcons-tail xs) newxs)))
  (define xs (make-mlist i j))
  (set-n-tail! xs xs)
  xs)

(define x (make-mlist 1 5))

(test)