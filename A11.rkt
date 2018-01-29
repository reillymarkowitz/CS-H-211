#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(struct Ball (x y sn sd) #:transparent) ; x-coordinate, y-coordinate, slope numerator, slope denominator
(struct World (p1 p2 s1 s2 b) #:transparent)
(define scale 35) ; [1, 49]

; point : WorldState Number -> WorldState
; Increments Player p's score, resets the ball and paddle positions
(define (point w p)
  (let ([B (struct-copy Ball (World-b w) [x 0] [y 0])])
    (if (= p 1)
        (World 0 0 (add1 (World-s1 w)) (World-s2 w) B)
        (World 0 0 (World-s1 w) (add1 (World-s2 w)) B))))

; ball-step : WorldState -> WorldState
; Continues to move the ball in its current direction
(define (ball-step w)
  (let ([B (World-b w)])
    (struct-copy World w [b (struct-copy Ball B
                                         [x (- (Ball-x B) (* (/ scale 5) (Ball-sd B)))]
                                         [y (- (Ball-y B) (* (/ scale 5) (Ball-sn B)))])])))

(define (paddle-bounce w)
  (ball-step
   (struct-copy World w
                [b (struct-copy Ball (World-b w)
                                [sd (* -1 (Ball-sd (World-b w)))])])))

(define (hit-wall? w)
  (let ([n (Ball-y (World-b w))])
    (or (> n (* 8.5 scale))
        (< n (* -8.5 scale)))))

(define (hit-paddle? w)
  (let ([B (World-b w)])
    (let ([x (Ball-x B)] [y (Ball-y B)])
      (or (and
           (> x (* 17.5 scale))
           (< (abs (- y (World-p1 w))) (* 3 scale)))
          (and
           (< x (* -17.5 scale))
           (< (abs (- y (World-p2 w))) (* 3 scale)))))))

(define (wall-bounce w)
  (ball-step
   (struct-copy World w
                [b (struct-copy Ball (World-b w)
                                [sn (* -1 (Ball-sn (World-b w)))])])))

; ball-handler : WorldState -> WorldState
; Updates the WorldState as follows:
;  - If Player 1 scores, (point w 1)
;  - If Player 2 scores, (point w 2)
;  - If the ball hits the top or bottom wall, bounce it off of the wall
;  - If the ball hits a paddle, bounce it off of the paddle
;  - Otherwise, (ball-step w)
(define (ball-handler w)
  (let ([B (World-b w)])
    (let ([x (Ball-x B)] [y (Ball-y B)])
      (cond
        [(< x (* -18.5 scale)) (point w 1)]
        [(> x (* 18.5 scale)) (point w 2)]
        [(hit-wall? w) (wall-bounce w)]
        [(hit-paddle? w) (paddle-bounce w)]
        [else (ball-step w)]))))

; key-handler : WorldState Key -> WorldState
; Updates the WorldState to move the paddles according to the key pressed by the user
(define (key-handler w k)
  (let ([p1 (World-p1 w)] [p2 (World-p2 w)])
    (cond
      [(and (equal? k "w") (< p1 (* scale 7))) (struct-copy World w [p1 (+ p1 (* scale 1.5))])]
      [(and (equal? k "s") (> p1 (* scale -7))) (struct-copy World w [p1 (- p1 (* scale 1.5))])]
      [(and (equal? k "up")(< p2 (* scale 7))) (struct-copy World w [p2 (+ p2 (* scale 1.5))])]
      [(and (equal? k "down") (> p2 (* scale -7))) (struct-copy World w [p2 (- p2 (* scale 1.5))])]
      [else w])))

(define (draw-World w) ; Draws the world by overlaying all of the UI elements
  (let ([paddle (rectangle (* scale 0.3) (* scale 4.5) "solid" "black")] [p1 (World-p1 w)] [p2 (World-p2 w)] [s1 (World-s1 w)] [s2 (World-s2 w)] [b (World-b w)])
    (overlay/offset paddle (* scale -18) p2
                    (overlay/offset paddle (* scale 18) p1
                                    (overlay/offset (square (* scale 1) "solid" "black") (Ball-x b) (Ball-y b)
                                                    (overlay/offset (text (number->string s2) (* scale 3) "black") (* scale -9.5) (* scale 7)
                                                                    (overlay/offset (text (number->string s1) (* scale 3) "black") (* scale 9.5) (* scale 7)
                                                                                    (overlay (let ([dash (rectangle (* scale 0.3) (* scale 1) "solid" "black")]) (let ([block (above dash (rectangle (* scale 0.3) (* scale 1) "solid" "white"))]) (above block block block block block block block block block dash))) (overlay (rectangle (* scale 38) (* scale 18) "solid" "white")
                                                                                                                                                                                                                                                                                                                                 (rectangle (* scale 40) (* scale 20) "solid" "black"))))))))))
(define w (World 0 0 0 0 (Ball 0 0 1 1)))

#;(big-bang w
  [on-tick ball-handler 0.01]
  [on-key key-handler]
  [to-draw draw-World])

; new-client : Universe IWorld -> Universe
; Adds the new client to our list of worlds.
(define (new-client u i)
  (cons u (append u (list i))))

; on-message : Universe IWorld Message -> Bundle
; Broadcasts the message received to all of the clients.
(define (on-message u from msg)
  (make-bundle (cons (cons (append (car (car u)) (list (cons (iworld-name from) msg))) (cdr (car u))) (cdr u))
               (map (λ(i) (make-mail i (list (iworld-name from) msg))) (cdr u))
               empty))

; receive-message : World Message -> World
; Updates the World state with the new message
(define (receive-message w m)
  (cons (append (car w) (list (cons (first m) (second m)))) (cdr w)))

(launch-many-worlds
 (universe (cons (cons empty "") empty)
           [on-new new-client]
           [on-msg on-message])
 (big-bang w
   [on-tick ball-handler 0.01]
   [to-draw draw-World]
   [on-key key-handler]
   [on-receive receive-message]
   [name "Player 1"]
   [register LOCALHOST])
 (big-bang w
   [on-tick ball-handler 0.01]
   [to-draw draw-World]
   [on-key key-handler]
   [on-receive receive-message]
   [name "Player 2"]
   [register LOCALHOST]))