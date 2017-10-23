#lang racket
(require math/number-theory)
(require test-engine/racket-tests)

;; procedure
;; (coprime? a b ...) → Boolean
;;   a : Integer
;;   b : Integer
;; Returns #t if the integers a b ... are coprime. Formally, a set of
;; integers is considered coprime (also called relatively prime) if
;; their greatest common divisor is 1.
;; Example:
;; > (coprime? 2 6 15)
;; #t

;; procedure
;; (modular-inverse a n) → Natural
;;   a : Integer
;;   n : Integer
;; Returns the inverse of a modulo n if a and n are coprime, otherwise
;; raises an error. The modulus n must be positive, and a must be
;; nonzero. Formally, if a and n are coprime, b = (modular-inverse a n)
;; is the unique natural number less than n such that (* a b) = 1 (mod n).
;; 
;; > (modular-inverse 2 5)
;; 3
;; > (modulo (* 2 3) 5)
;; 1

;; procedure
;; (modular-expt a b n) → Natural
;;   a : Integer
;;   b : Integer
;;   n : Integer
;; Computes (modulo (expt a b) n), but much more efficiently. The modulus
;; n must be positive, and the exponent b must be nonnegative.
;; Examples:
;; > (modulo (expt -6 523) 19)
;; 13
;; > (modular-expt -6 523 19)
;; 13
;; > (modular-expt 9 158235208 19)
;; 4
;; > ; don't try this at home!
;;   (modulo (expt 9 158235208) 19)
;; 4

;;---------------------------------------------------------------------------
;; Problem 1

;; procedure
;; (middle-digit a) -> Natural
;;   a : Natural in the range [100,999]
;; Computes the middle digit using quotient and modulo
;; Examples:
;; > (middle-digit 123)
;; 2
;; > (middle-digit 101)
;; 0
;; > (middle-digit 979)
;; 7

(define (middle-digit a) (modulo (quotient a 10) 10))

(check-expect (middle-digit 123) 2)
(check-expect (middle-digit 101) 0)
(check-expect (middle-digit 979) 7)
;;---------------------------------------------------------------------------
;; Problem 2

;; procedure
;; (new-arrival-time original-time delay) -> Natural
;;   original-time : Natural number representing a time in hours
;;   delay : Natural representing a delay in hours
;; Computes the new arrival time (ignoring the am/pm distinction)
;; Examples
;; > (new-arrival-time 3 14)
;; 5
;; > (new-arrival-time 5 2)
;; 7
;; > (new-arrival-time 11 7)
;; 6
;; > (new-arrival-time 3 1000)
;; 7

(define (tool x y z)
  (modulo (+ x y) z))

(define (new-arrival-time original-time delay)
  (tool original-time delay 12))

(check-expect (new-arrival-time 3 14) 5)
(check-expect (new-arrival-time 5 2) 7)
(check-expect (new-arrival-time 11 7) 6)
(check-expect (new-arrival-time 3 1000) 7)
;;---------------------------------------------------------------------------
;; Problem 3

;; procedure
;; (day-of-week today n) -> Natural in the range [0,6]
;;   today : Natural in the range [0,6] with 0=Sunday, 1=Monday, etc.
;;   n : Natural representing a number of days
;; Computes the new day of week after n days
;; Examples
;; > (day-of-week 1 0)
;; 1
;; > (day-of-week 1 1)
;; 2
;; > (day-of-week 1 7)
;; 1
;; > (day-of-week 1 10000)
;; 5

(define (day-of-week today n)
  (tool today n 7))

(check-expect (day-of-week 1 0) 1)
(check-expect (day-of-week 1 1) 2)
(check-expect (day-of-week 1 7) 1)
(check-expect (day-of-week 1 10000) 5)
;;---------------------------------------------------------------------------
;; Problem 4

;; procedure
;; (compute-public-key base private-key modulus) -> Natural
;;   base : Natural that is prime
;;   private-key : Natural
;;   modulus : Natural that is prime
;; Computes base raised to the power of private-key modulo modulus
;; Examples
;; > (compute-public-key 5 6 23)
;; 8
;; > (compute-public-key 5 15 23)
;; 19


(define (compute-public-key base private-key modulus) (modular-expt base private-key modulus))

(check-expect (compute-public-key 5 6 23) 8)
(check-expect (compute-public-key 5 15 23) 19)
                                                         
;;---------------------------------------------------------------------------
;; Problem 5

;; procedure
;; (sign-with-private-key n private-key modulus) -> Natural
;;   n : Natural
;;   private-key : Natural
;;   modulus : Natural that is prime
;; Computes n raised to the power of private-key modulo modulus
;; Examples
;; > (sign-with-private-key 19 6 23)
;; 2
;; > (sign-with-private-key 8 15 23)
;; 2

(define (sign-with-private-key n private-key modulus) (modular-expt n private-key modulus))

(check-expect (sign-with-private-key 19 6 23) 2)
(check-expect (sign-with-private-key 8 15 23) 2)
;;---------------------------------------------------------------------------
;; Problem 6

;; For this problem, I chose two small prime numbers and generated the public
;; key:
;; e = 5
;; n = 21
;; Here are several values encoded using this public key:
;; > (encode 3)
;; 12
;; > (encode 9)
;; 18
;; > (encode 10)
;; 19
;;
;; Write the following procedure

;; procedure
;; (generate-conflict x) -> Natural
;;   x : Natural
;; Returns a Natural number m such that (encode m) is idential to (encode x).
;; There are multiple possible answers. Any answer that causes a conflict is
;; acceptable
;; Examples
;; > (generate-conflict 3)
;; 108
;; > (encode 108)
;; 12
;; > (generate-conflict 9)
;; 114
;; > (encode 114)
;; 18
;; > (generate-conflict 10)
;; 115
;; > (encode 115)
;; 19

(define e 5)
(define n 21)
(define (encode x) (modular-expt x e n))
(define (generate-conflict x) (+ n x))

(check-expect (encode(generate-conflict 3)) (encode 3))
(check-expect (encode 108) 12)
(check-expect (encode(generate-conflict 9)) (encode 9))
(check-expect (encode 114) 18)
(check-expect (encode(generate-conflict 10)) (encode 10))
(check-expect (encode 115) 19)
;;---------------------------------------------------------------------------
;; Problem 7

;; procedure
;; (encode-middle-digit n) -> Natural
;;   n : Natural in the range [100,999]
;; Extracts the middle digit of n and encodes it using the public key of the
;; previous problem
;; Examples
;; > (encode-middle-digit 131)
;; 12
;; > (encode-middle-digit 198)
;; 18

(define (encode-middle-digit n) (encode (middle-digit n)))
(check-expect (encode-middle-digit 131) 12)
(check-expect (encode-middle-digit 198) 18)
;;---------------------------------------------------------------------------
(test)