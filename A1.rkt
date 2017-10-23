#lang racket
(require 2htdp/image)

; Reilly Markowitz
; CS H-211
; A1
; 8/24/17

(define small-red-circle (circle 5 "solid" "Firebrick"))
(define small-white-circle (circle 5 "solid" "White"))
(define tri (triangle 25 "outline" "Firebrick"))

; Serpenski Gasket

(define small-gasket (above tri (beside tri tri)))
(define med-gasket (above small-gasket (beside small-gasket small-gasket)))
(define huge-gasket (above med-gasket (beside med-gasket med-gasket)))

; Serpenski Carpet

(define small-square (square 5 "solid" "Gray"))
(define small-carpet-spacer (square 5 "solid" "White"))
(define med-carpet-spacer (square 15 "solid" "White"))
(define huge-carpet-spacer (square 45 "solid" "White"))

(define (line-maker square) (beside square square square))
(define (carpet-maker square spacer) (above (above (line-maker square) (beside square spacer square))(line-maker square)))
(define small-carpet (carpet-maker small-square small-carpet-spacer))
(define med-carpet (carpet-maker small-carpet med-carpet-spacer))
(define huge-carpet (carpet-maker med-carpet huge-carpet-spacer))

; Trains

(define wheel (circle 5 "solid" "Dark Gray"))
(define big-red-square (square 20 "solid" "Firebrick"))
(define big-blue-square (square 20 "solid" "Navy"))
(define big-green-square (square 20 "solid" "OliveDrab"))

(define wheels (beside wheel small-white-circle small-white-circle small-white-circle wheel))
(define hat (beside small-square small-square small-square small-square small-square))
(define (car-maker square) (above (beside square square square) wheels))
(define train (beside (car-maker big-red-square) hat (car-maker big-blue-square) hat (car-maker big-green-square)))

; Castles

(define small-white-spacer (square 5 "solid" "White"))
(define big-dg-square (square 20 "solid" "Dark Gray"))
(define big-white-spacer (square 20 "solid" "White"))

(define crenulation (beside small-square small-white-spacer small-square))
(define tower (above crenulation big-dg-square big-dg-square big-dg-square))
(define base (above crenulation big-dg-square big-dg-square))
(define castle (beside/align "bottom" tower tower base base tower tower))