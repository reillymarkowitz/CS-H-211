#lang racket

(require test-engine/racket-tests) ;; For check/expect
(require 2htdp/image)              ;; For images
(require 2htdp/universe)           ;; For big-bang, etc
(require srfi/13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ADIT: SCROLL TO THE BOTTOM FOR INSTRUCTIONS;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------------------------------------------------------------------------;
;                               1. Soda Machine                                ;
;------------------------------------------------------------------------------;

(define NICKEL 5)
(define DIME 10)
(define PRICE 15)

(define bal 0)    ; The initial balance of the machine is 0 cents. This will change when the user deposits nickels and dimes

; can-purchase? is one of:
; #t
; #f
(define can-purchase? #f) ; The user can't purchase a soda yet. This will change when there is at least 15 cents in the machine

(define insert-n "n")     ; The user presses the 'n' key to deposit a nickel
(define insert-d "d")     ; The user presses the 'd' key to deposit a dime
(define purchase " ")     ; The user presses the spacebar to purchase a soda using the balance

; int -> boolean
; Takes amount in cents and determines if it's enough to buy a soda. Sets can-purchase to the answer to that question
(define (soda-gatekeeper x)
   (set! can-purchase? (>= x PRICE)))

; int -> int
; Takes an amount int cents and adds it to the current balance. Then returns the balance
(define (add-bal x)
  (set! bal (+ bal x))
  bal)

; int -> int
; Takes an amount in cents, checks if it's enough to purchase, and goes through with the purchase if possible. Returns balance
(define (purchase-soda x)
  (soda-gatekeeper x)
  (if can-purchase? (set! bal (- bal 15)) (text "INSUFFICIENT FUNDS" 40 "red"))
  bal)

; key -> int
; Takes user input and either deposits a nickel, deposits a dime, or makes a purchase based on the input
(define (soda-actions x key)
  (cond
    [(equal? key insert-n) (add-bal NICKEL)]
    [(equal? key insert-d) (add-bal DIME)]
    [(equal? key purchase) (purchase-soda x)]))

; int -> text
; Creates the user interface and outputs it to the screen
(define (soda-render ms)
  (text (string-append (number->string bal) "                      ") 40 "green"))

; int -> UI
; Simulates the soda machine
(define (soda-sim bal)
  (big-bang bal
            [on-key soda-actions]
            [to-draw soda-render]))

;------------------------------------------------------------------------------;
;                           2. Combination Lock                                ;
;------------------------------------------------------------------------------;

(define COMBO 135) ; The lock combination

(define ld (list (quotient COMBO 100) (modulo (quotient COMBO 10) 10) (modulo COMBO 10))) ; List containing the digits of the combination, in order.

; A LockState is one of
; b1
; b2
; b3
; UNLOCKED
(define b1 0)       ; State #1, locked, no digits guessed correctly
(define b2 1)       ; State #2, locked, first digit guessed correctly
(define b3 2)       ; State #3, locked, first and second digits guessed correctly
(define UNLOCKED 3) ; State #4, unlocked, all three digits guessed correctly

; Void -> integer
; Counts the number of correct digits guessed in a row
(define lock-count 0) 
(define (lock-actions ls key) 
  (for ([i 3])          ; For each digit in the combination, if the user guessed the digit correctly, increment lock-count. Otherwise, start over from 0
        (cond [(= ls i) (cond [(equal? key (number->string (list-ref ld i))) (set! lock-count (+ lock-count 1))] [else (set! lock-count 0)])]))
  lock-count)           ; Return lock-count

(check-expect (lock-actions b1 "1") 1)
(check-expect (lock-actions b2 "3") 2)
(check-expect (lock-actions b3 "7") 0)
(check-expect (lock-actions b1 "1") 1)
(check-expect (lock-actions b2 "3") 2)
(check-expect (lock-actions b3 "5") 3)

; LockState -> text
; Renders the user interface.
(define (lock-render ls)
  (cond
    [(= ls b1) (text "LOCKED     " 40 "red")]        ; Displays "LOCKED" in red if in State #1
    [(= ls b2) (text "LOCKED     " 40 "orange")]     ; Displays "LOCKED" in orange if in State #2
    [(= ls b3) (text "LOCKED     " 40 "yellow")]     ; Displays "LOCKED" in yellow if in State #3
    [(= ls UNLOCKED) (text "UNLOCKED" 40 "green")])) ; Displays "UNLOCKED" in green if in State #4

(check-expect (lock-render b1) (text "LOCKED     " 40 "red"))
(check-expect (lock-render b2) (text "LOCKED     " 40 "orange"))
(check-expect (lock-render b3) (text "LOCKED     " 40 "yellow"))
(check-expect (lock-render UNLOCKED) (text "UNLOCKED" 40 "green"))

; LockState -> UI
; Simulates the combination lock
(define (lock-sim ls)
  (big-bang ls
            [on-key lock-actions]
            [to-draw lock-render]))

;------------------------------------------------------------------------------;
;                               3. String Parse                                ;
;------------------------------------------------------------------------------;

(define words "so~soot~soup~san~sand~sound~salt")
(define wordlist (list "so" "soot" "soup" "san" "sand" "sound" "salt"))
(define str "")
; A StringState is one of
; MISSPELLED
; ON-TRACK
; WHOLE-WORD
(define MISSPELLED 0)
(define ON-TRACK 1)
(define WHOLE-WORD 2)

; key --> StringState
; Adds user input to the current string and determines the StringState
(define (string-actions ss key)
  (set! str (string-append str key))
  (cond
    [(member str wordlist) WHOLE-WORD]
    [(string-contains words str) ON-TRACK]
    [(not (string-contains words str)) (set! str "") MISSPELLED]))

(check-expect (string-actions ON-TRACK "soot") WHOLE-WORD)
(check-expect (string-actions MISSPELLED "soo") MISSPELLED)
(check-expect (string-actions ON-TRACK "sfd") MISSPELLED)

; StringState --> text
; Takes the StringState and outputs the appropriate text to the UI
(define (string-render ss)
  (if (= ss WHOLE-WORD) (text (string-append str "          ") 40 "green") (text (string-append str "          ") 40 "yellow")))

; StringState --> UI
; Runs the String Parse simulation
(define (string-sim ss)
  (big-bang ss
            [on-key string-actions]
            [to-draw string-render]))

;------------------------------------------------------------------------------;
;                               4. Thermometer                                 ;
;------------------------------------------------------------------------------;

; Temperature can be any integer t where t = 70 + [15 x (any integer)]
(define temp 70)

; Integer --> Integer
; Takes the current temperature and changes it to +- 15 randomly.
(define (temp-changer t)
  (if (= (random 2) 0) (set! temp (- temp 15)) (set! temp (+ temp 15))) temp)

; Integer --> text
; Takes the temperature and outputs the appropriate text to the UI
(define (temp-render t)
  (define description "")
  (cond
    [(< t 32) (set! description "FREEZING ")]
    [(and(>= t 32) (<= t 64)) (set! description "cool ")]
    [(and(>= t 65) (<= t 80)) (set! description "balmy ")]
    [else (set! description "HOT ")])
  (text (string-append "It's a " description (number->string t) " degrees!                 ") 40 "black"))

(check-expect (temp-render 31) (text (string-append "It's a FREEZING " "31" " degrees!                 ") 40 "black"))
(check-expect (temp-render 60) (text (string-append "It's a cool " "60" " degrees!                 ") 40 "black"))
(check-expect (temp-render 75) (text (string-append "It's a balmy " "75" " degrees!                 ") 40 "black"))
(check-expect (temp-render 85) (text (string-append "It's a HOT " "85" " degrees!                 ") 40 "black"))

; Integer --> UI
; Runs the thermometer simulation
(define (temp-sim t)
  (big-bang t
            [on-tick temp-changer 3]
            [to-draw temp-render]))
;------------------------------------------------------------------------------;
;                              5. Bag of Marbles                               ;  ;;Still working
;------------------------------------------------------------------------------;

(define REFILL ".")
(define TAKE " ")

; BagState can be one of the following, without regard for order:
; RGB
; RG
; RB
; GB
; R
; G
; B
(define bag "RGB")

; Void -> BagState
; Refills the bag and then returns the new BagState
(define (refill-bag) (set! bag "RGB") bag)

; Void -> BagState
; Removes a random marble and returns the new BagState
(define (remove-rand)
  (define x -1)
  (cond [(> (string-length bag) 0) (set! x (random (string-length bag))) (set! bag (string-append (substring bag 0 x) (substring bag (+ x 1))))])
  bag)

; Key -> BagState
; Processes the user input and responds appropriately
(define (marble-actions bag key)
  (cond
    [(equal? key TAKE) (remove-rand)]
    [(equal? key REFILL) (refill-bag)]
    [else (error "Unrecognized key.")]))

; BagState -> text
; Outputs the appropriate text to the UI
(define (marble-render bag)
  (if (void? bag) (text "ERR" 40 "black") (text bag 40 "black")))

(check-expect (marble-render (void bag)) (text "ERR" 40 "black"))
(check-expect (marble-render bag) (text bag 40 "black"))

; BagState -> UI
; Runs the Bag of Marbles simulation
(define (marble-sim bag)
  (big-bang bag
            [on-key marble-actions]
            [to-draw marble-render]))

;------------------------------------------------------------------------------;
;                               6. Useless Box                                 ;
;------------------------------------------------------------------------------;

; A BoxState is one of
; #t
; #f

; BoxState -> boolean
; Changes the BoxState to #f
(define (box-closer bs) #f)

(check-expect (box-closer #t) #f)

; BoxState, key -> boolean
; If the box is open and the user presses the spacebar, close the box.
(define (box-actions bs key) (cond [(and bs (equal? key " ")) #f]))

(check-expect (box-actions #t " ") #f)

; BoxState -> text
; Renders the text for the UI
(define (box-render bs) (if bs (text "OPEN" 40 "green") (text "CLOSED" 40 "red")))

; BoxState -> UI
; Runs the Useless Box simulation
(define (box-sim bs) (big-bang bs [on-tick box-closer 5] [on-key box-actions] [to-draw box-render]))

;------------------------------------------------------------------------------;
;                               7. Coffee Pot                                  ;
;------------------------------------------------------------------------------;

(define POT-SIZE 5) ; A pot of coffee can hold 5 cups
(define pot POT-SIZE) ; The pot is full to begin with

; A PotState is one of
; EMPTY
; HAS-COFFEE
; FULL
; OVERFLOW
; BROKEN
(define EMPTY 0)
(define HAS-COFFEE 1)
(define FULL 2)
(define OVERFLOW 3)
(define BROKEN 4)

(define BREW! "b")
(define TAKE! "t")
(define CLEAN! "c")
(define FIX! "f")

; PotState -> PotState
; Takes the current PotState, brews a new pot of coffee, and returns the new PotState
(define (brew-pot ps)
  (set! pot (+ POT-SIZE pot))
  (cond
    [(equal? pot POT-SIZE) FULL]
    [(> pot POT-SIZE) OVERFLOW]
    [else HAS-COFFEE]))

; PotState -> PotState
; Takes the current PotState, takes a cup of coffee if possible, and returns the new PotState
(define (take-cup ps)
  (cond
    [(equal? ps EMPTY) EMPTY]
    [(or (equal? ps HAS-COFFEE) (equal? ps FULL)) (set! pot (sub1 pot)) (if (> pot 0) HAS-COFFEE EMPTY)] ; If the state is FULL or HAS-COFFEE, take away a cup. If there is any coffee left in the pot after that, set the state to HAS-COFFEE. Else set the state to EMPTY.
  ))

(check-expect (take-cup EMPTY) EMPTY)

; Resets the pot
(define (reset-pot)
  (set! pot 0)
  EMPTY)

; PotState -> PotState
; If the PotState is empty, break the pot. Else leave it as is
(define (breaker ps)
  (if (equal? ps EMPTY) BROKEN ps))

(check-expect (breaker EMPTY) BROKEN)
(check-expect (breaker FULL) FULL)
(check-expect (breaker HAS-COFFEE) HAS-COFFEE)
(check-expect (breaker BROKEN) BROKEN)
(check-expect (breaker OVERFLOW) OVERFLOW)

; PotState -> PotState
; Takes the appropriate action based on user input and the current PotState, and returns the new PotState
(define (coffee-actions ps key)
  (cond
    [(equal? key BREW!) (brew-pot ps)]
    [(equal? key TAKE!) (take-cup ps)]
    [(equal? key CLEAN!) (reset-pot)]
    [(equal? key FIX!) (reset-pot)]))

; PotState -> text
; Outputs the appropriate text to the UI
(define (coffee-render ps)
  (define condition "")
  (cond
    [(equal? ps EMPTY) (set! condition "The pot is empty. Press \"B\" to brew coffee.")]
    [(equal? ps FULL) (set! condition "The pot is full. Press \"T\" to take a cup of coffee.")]
    [(equal? ps OVERFLOW) (set! condition "The pot is overflowing. Press \"C\" to clean it up.")]
    [(equal? ps BROKEN) (set! condition "The pot is broken. Press \"F\" to fix it.")])
  (text (string-append "Cups in pot: " (number->string pot) ". " condition "         ") 40 "black"))

(check-expect (coffee-render EMPTY) (text (string-append "Cups in pot: " (number->string pot) ". " "The pot is empty. Press \"B\" to brew coffee." "         ") 40 "black"))
(check-expect (coffee-render FULL) (text (string-append "Cups in pot: " (number->string pot) ". " "The pot is full. Press \"T\" to take a cup of coffee." "         ") 40 "black"))
(check-expect (coffee-render OVERFLOW) (text (string-append "Cups in pot: " (number->string pot) ". " "The pot is overflowing. Press \"C\" to clean it up." "         ") 40 "black"))
(check-expect (coffee-render BROKEN) (text (string-append "Cups in pot: " (number->string pot) ". " "The pot is broken. Press \"F\" to fix it." "         ") 40 "black"))
(check-expect (coffee-render HAS-COFFEE) (text (string-append "Cups in pot: " (number->string pot) ". " "         ") 40 "black"))

; PotState -> UI
; Runs the coffee machine simulation
(define (coffee-sim ps)
  (big-bang ps
            [on-key coffee-actions]
            [to-draw coffee-render]
            [on-tick breaker 5]))

;------------------------------------------------------------------------------;
;                               INSTRUCTIONS                                   ;
;------------------------------------------------------------------------------;

; FOR QUICK ACCESS, UN-COMMENT THE LINE THAT CALLS THE FUNCTION YOU WANT TO CALL BELOW, THEN RE-COMMENT THE LINE WHEN FINISHED

; Test all (check-expect) calls.
; (test)

; 1. Soda Machine            ; Press 'n' to deposit a nickel, 'd' to deposit a dime, and spacebar to purchase a soda for 15 cents
; (soda-sim bal)

; 2. Combination Lock
; (lock-sim b1)

; 3. String Parse
; (string-sim MISSPELLED)

; 4. Thermometer
; (temp-sim temp)

; 5. Bag of Marbles          ; Press spacebar to take a random marble, and '.' to refill the bag
; (marble-sim bag)

; 6. Useless Box             ; Press spacebar to open the box, or to close the box before it closes itself
; (box-sim #f)

; 7. Coffee Pot              ; Press 'b' to brew a pot of coffee, 't' to take a cup, 'f' to fix the machine when it breaks, 
; (coffee-sim FULL)          ; and 'c' to clean the machine when it overflows
