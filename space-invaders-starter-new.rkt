;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter-new) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvaders is one of:
;; - empty
;; - (cons Invader ListOfInvaders)
;; interp. a list containing invaders

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
    (cond [(empty? loi)(...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules Used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvaders)
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListOfInvaders

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissiles is one of:
;; - empty
;; - (cons Missile ListOfMissles)
;; interp. a list containing missles

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
    (cond [(empty? lom)(...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules Used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissiles)
;; - reference: (first loi) is Missile
;; - self-reference: (rest loi) is ListOfMissiles

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ===============================
;; Functions:

;; Game -> Game
;; start game program by evaluating (main G0)

(define (main g)
  (big-bang g
    (on-tick  next-game)
    (to-draw  render-game)
    (on-key   handle-keys)
    (stop-when game-over)
    )
  )

;; Game -> Game
;; produce next state of game, creating and moving items forward
;; unable to test due to random condition to make invaders

;(define (next-game g) T0) ;stub

;<template from Game>
(define (next-game g)
  (make-game (make-invaders (next-invaders (remove-invaders (game-invaders g)(game-missiles g))))
             (next-missiles (remove-missiles (game-missiles g)(game-invaders g)))
             (next-tank (game-tank g)))
  )

;; Game -> Image
;; render the game onto BACKGROUND
(check-expect (render-game G0) (place-image TANK (tank-x T0) TANK-Y BACKGROUND))
(check-expect (render-game G2) (place-image INVADER (invader-x I1) (invader-y I1)
                                            (place-image MISSILE (missile-x M1) (missile-y M1)
                                                         (place-image TANK (tank-x T1) TANK-Y BACKGROUND))))

;(define (render-game g) BACKGROUND) ;stub

;<template from Game>
(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g))))
  )

;; Game -> Boolean
;; produce game over state and ends the game if lost? is true
(check-expect (game-over G0) false)
(check-expect (game-over (make-game (list I1 (make-invader 10 10 -12)) empty T1)) false)
(check-expect (game-over (make-game (list I1 (make-invader 10 HEIGHT -12)) empty T1)) true)

;(define (game-over g) false) ;stub

;<template from Game>
(define (game-over g)
  (lost? (game-invaders g))
  )

;; ListOfInvaders -> Boolean
;; produce true if one of the invaders in ListOfInvaders reaches the bottom of the BACKGROUND (invader-y is HEIGHT)
(check-expect (lost? LOI0) false)
(check-expect (lost? (list I1 (make-invader 10 10 -12))) false)
(check-expect (lost? (list I1 (make-invader 10 HEIGHT -12))) true)

;(define (lost? loi) false) ;stub

;<template from ListOfInvaders>
(define (lost? loi)
  (cond [(empty? loi) false]
        [(>= (invader-y (first loi)) HEIGHT) true]
        [else
         (lost? (rest loi))]))

;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; remove missiles that hit an invader (missile and invader x,y position are within HIT-RANGE of each other) from the list of missiles
(check-expect (remove-missiles LOM0 LOI0) LOM0)
(check-expect (remove-missiles (list M1 M2) (list I1 (make-invader 150 310 12))) empty)

;(define (remove-missiles loi lom) LOM0) ;stub

;<template from ListOfMissiles w/ extra list parameter>
(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (hit-missile? (first lom) loi)
             (remove-missiles (rest lom) loi)
             (cons (first lom) (remove-missiles (rest lom) loi)))]))

;; Missile ListOfInvaders -> Boolean
;; produce true if given missile has hit an invader (missile and invader x,y position are within HIT-RANGE of each other)
(check-expect (hit-missile? M1 (list I1 (make-invader 150 312 5)))
              false)
(check-expect (hit-missile? M1 (list I1 (make-invader 150 310 5)))
              true)

;(define (hit-missile? m loi) true) ;stub

;<template from ListOfInvaders w/ extra compound parameter>
(define (hit-missile? m loi)
  (cond [(empty? loi) false]
        [else
         (if (in-range? m (first loi))
             true
             (hit-missile? m (rest loi)))]))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; remove invaders that have been hit by missile (missile and invader x,y position are within HIT-RANGE of each other) from the list of invaders
(check-expect (remove-invaders LOI0 LOM0) LOI0)
(check-expect (remove-invaders (list I1 (make-invader 150 310 12)) (list M1 M2)) empty)

;(define (remove-invaders loi lom) LOI0) ;stub

;<template from ListOfInvaders w/ extra list parameter>
(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (hit-invader? (first loi) lom)
             (remove-invaders (rest loi) lom)
             (cons (first loi) (remove-invaders (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; produce true if given invader has been hit by missile (missile and invader x,y position are within HIT-RANGE of each other)
(check-expect (hit-invader? I1 (list M1 (make-missile 150 112)))
              false)
(check-expect (hit-invader? I1 (list M1 (make-missile 150 110)))
              true)

;(define (hit-invader? i lom) true) ;stub

;<template from Invader w/ extra list parameter>
(define (hit-invader? i lom)
  (cond [(empty? lom) false]
        [else
         (if (in-range? (first lom) i)
             true
             (hit-invader? i (rest lom)))]))

;; Missile Invader -> Boolean
;; produce true if given missile is within HIT-RANGE of given invader
(check-expect (in-range? M1 I1) false)
(check-expect (in-range? M1 (make-invader 150 310 5)) true)

;(define (in-range? m i) false) ;stub

;<template from Missile w/ extra compound parameter>
(define (in-range? m i)
  (if (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
           (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE))
      true
      false))

;; ListofInvaders -> ListofInvaders
;; produce filtered and ticked list of invaders
(check-expect (next-invaders LOI0) LOI0)
(check-expect (next-invaders LOI1) (list (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 (* 12 INVADER-Y-SPEED)) 12)))

;(define (next-invaders loi) LOI0) ;stub

;<template from function composition>
(define (next-invaders loi)
  (onscreen-invaders (tick-invaders loi)))

;; ListOfInvaders -> ListOfInvaders
;; produce list of ticked invaders
(check-expect (tick-invaders LOI0) empty)
(check-expect (tick-invaders LOI1) (list (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 (* 12 INVADER-Y-SPEED)) 12)))

;(define tick-invaders loi) LOI0) ;stub

;<template from ListOfInvaders>
(define (tick-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (tick-invader (first loi))
               (tick-invaders (rest loi)))])
  )

;; Invader -> Invader
;; produce a new invader that is one pixel farther down the screen in invader-dx direction
(check-expect (tick-invader I1) (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 (* 12 INVADER-Y-SPEED)) 12))
(check-expect (tick-invader I2) (make-invader (+ 150 (* -10 INVADER-X-SPEED)) (+ HEIGHT (* 10 INVADER-Y-SPEED)) -10))
(check-expect (tick-invader I3) (make-invader (+ 150 (* 10 INVADER-X-SPEED)) (+ (+ 10 HEIGHT) (* 10 INVADER-Y-SPEED)) 10))

;(define (tick-invader i) I0) ;stub

;<template from Invader>
(define (tick-invader i)
  (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED)) (invader-dx i))
  )

;; ListOfInvaders -> ListOfInvaders
;; produce a list containing only those invaders in loi that are onscreen (within [0, WIDTH])
(check-expect (onscreen-invaders LOI0) empty)
(check-expect (onscreen-invaders LOI2) LOI2)
(check-expect (onscreen-invaders (list (make-invader (+ 1 WIDTH) 100 12) I1 I2)) (list (make-invader WIDTH 100 -12) I1 I2))

;(define (onscreen-invaders loi) LOI0) ;stub

;<template from ListOfInvaders>
(define (onscreen-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (edge-invaders (first loi))
               (onscreen-invaders (rest loi)))]))

;; Invader -> Invader
;; produce a direction change for invader when given invader x,y position is at screen edges
(check-expect (edge-invaders I1) (make-invader 150 100 12))
(check-expect (edge-invaders (make-invader (+ 1 WIDTH) 100 12)) (make-invader WIDTH 100 -12))
(check-expect (edge-invaders (make-invader (- 0 1) 100 -12)) (make-invader 0 100 12))

;(define (edge-invaders t) I1) ;stub

;<template from Invader>
(define (edge-invaders i)
  (cond [(< (invader-x i) 0) (make-invader 0 (invader-y i) (- (invader-dx i)))]
        [(> (invader-x i) WIDTH) (make-invader WIDTH (invader-y i) (- (invader-dx i)))]
        [else
         (make-invader (invader-x i) (invader-y i) (invader-dx i))]))

;; ListOfInvaders -> Image
;; Render the invaders onto BACKGROUND at position invader-x, invader-y
(check-expect (render-invaders LOI0 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1)
                                                             (place-image INVADER (invader-x I2) (invader-y I2)
                                                                          (place-image INVADER (invader-x I3) (invader-y I3) BACKGROUND))))
;(define (render-invaders loi img) BACKGROUND) ;stub

;<template from ListOfInvaders w/ extra atomic parameter>
(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else 
         (place-invader (first loi)
                        (render-invaders (rest loi) img))]))


;; Invader Image -> Image
;; place invader on img as specified by i
(check-expect (place-invader I1 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))

;(define (place-invader i img) BACKGROUND) ;stub

;<template from Invader w/ extra atomic parameter>
(define (place-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; ListOfInvaders -> ListOfInvaders
;; add invaders to ListOfInvaders randomly based on INVADER-RATE (invaders start at random x position within WIDTH and y = 0 with random direction)
;; unable to test due to random condition

;(define (make-invaders loi) LOI0) ;stub

;<template from ListOfInvaders>
(define (make-invaders loi)
  (if (< (random INVADE-RATE) 3)
      (cons (make-invader (random WIDTH) 0 (random-dir (random 2))) loi)        
      loi))

;; Integer -> Integer
;; produce -1 if given value is odd, else produce 1
(check-expect (random-dir 2) 1)
(check-expect (random-dir 1) -1)

;(define (random-dir i) 1) ; stub

(define (random-dir i)
  (if (odd? i)
      (- 1)
      1))

;; ListOfMissiles -> ListOfMissiles
;; produce filtered and ticked list of missiles
(check-expect (next-missiles LOM0) LOM0)
(check-expect (next-missiles (list M1 (make-missile 150 0))) (list (make-missile 150 (- 300 MISSILE-SPEED))))

;(define (next-missiles lom) LOM0) ;stub

;<template from function composition>
(define (next-missiles lom)
  (onscreen-missiles (tick-missiles lom)))

;; ListOfMissiles -> ListOfMissiles
;; produce list of ticked missiles
(check-expect (tick-missiles LOM0) LOM0)
(check-expect (tick-missiles (list (make-missile 150 300)(make-missile 190 200)))
              (list (make-missile 150 (- 300 MISSILE-SPEED))
                    (make-missile 190 (- 200 MISSILE-SPEED))))

;(define (tick-missiles lom) LOM0) ;stub

;<template from ListOfMissiles>
(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else 
         (cons (tick-missile (first lom))
               (tick-missiles (rest lom)))]))

;; Missile -> Missile
;; produce a new missile that is one pixel farther up the screen
(check-expect (tick-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (tick-missile (make-missile 150 0)) (make-missile 150 (- 0 MISSILE-SPEED)))

;(define (tick-missile m) M1) ;stub

;<template from Missile>
(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissiles -> ListOfMissiles
;; produce a list containing only those missiles in lom that are onscreen
(check-expect (onscreen-missiles LOM0) LOM0)
(check-expect (onscreen-missiles LOM1) (list (make-missile 150 300)))
(check-expect (onscreen-missiles (list M1 (make-missile 150 -1))) (list M1))

;(define (onscreen-missiles lom) LOM0) ;stub

;<template from ListOfMissiles>
(define (onscreen-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (edge-missile? (first lom))
             (cons (first lom) (onscreen-missiles (rest lom)))
             (onscreen-missiles (rest lom)))]))


;; Missile -> Boolean
;; produce true if m has not travelled past the top of BACKGROUND
(check-expect (edge-missile? M1) true)
(check-expect (edge-missile? (make-missile 150 -1)) false)

;(define (edge-missile? m) false) ;stub

;<template from Missile>
(define (edge-missile? m)  
  (<= 0 (missile-y m)))


;; ListOfMissiles -> Image
;; Render the missiles onto BACKGROUND
(check-expect (render-missiles LOM0 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))

;(define (render-missiles lom img) BACKGROUND) ;stub
;<template from ListOfMissiles w/ extra atomic parameter>

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else 
         (place-missile (first lom)
                        (render-missiles (rest lom) img))]))

;; Missiles Image -> Image
;; place missile on img as specified by m
(check-expect (place-missile M1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (place-missile M3 BACKGROUND) (place-image MISSILE (missile-x M3) (missile-y M3) BACKGROUND))

;(define (place-missile m img) BACKGROUND) ;stub

;<template from Missile w/ extra atomic parameter>
(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; Tank -> Tank
;; produce next tank, by advancing it TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
(check-expect (next-tank T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (next-tank T2) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (next-tank (make-tank (+ WIDTH 1) 1)) (make-tank WIDTH -1))
(check-expect (next-tank (make-tank -1 -1)) (make-tank 0 1))

;(define (next-tank t) T) ;stub

;<template from function composition>
(define (next-tank t)
  (edge-tank(advance-tank t))
  )

;; Tank -> Tank
;; advance tank by TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
(check-expect (advance-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (advance-tank (make-tank (/ WIDTH 2) -1)) (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))

;(define (advance-tank t) T0) ;stub

;<template from Tank>
(define (advance-tank t)
    (if (= (tank-dir t) 1)
      (make-tank (+ (tank-x t) TANK-SPEED) 1)
      (make-tank (- (tank-x t) TANK-SPEED) -1))) 

;; Tank -> Tank
;; when tank gets to edge, change dir to dir -1 if at right edge (WIDTH), and dir 1 if at left edge (0)
(check-expect (edge-tank T0) T0)
(check-expect (edge-tank (make-tank (+ 1 WIDTH) 1)) (make-tank WIDTH -1))
(check-expect (edge-tank (make-tank -1 -1)) (make-tank 0 1))

;(define (edge-tank t) T0) ;stub

;<template from Tank>
(define (edge-tank t)
  (cond [(> (tank-x t) WIDTH) (make-tank WIDTH -1)]
        [(< (tank-x t) 0) (make-tank 0 1)]
        [else
         (make-tank (tank-x t) (tank-dir t))])
  )


;; Tank -> Image
;; place appropriate tank image on BACKGROUND at (tank-x t) and HEIGHT - TANK-HEIGHT/2
(check-expect (render-tank T0) (place-image TANK (tank-x T0) TANK-Y BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

;<template from Tank>
(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND)
  )

;; Game KeyEvent -> Game
;; handle key presses, left arrow key moves tank left (dir -1), right arrow key moves tank right (dir 1), 
;;                     space key creates missile at tank-x t and tank y position
(check-expect (handle-keys G0 "up") G0)
(check-expect (handle-keys G0 "down") G0)
(check-expect (handle-keys G0 "left") (make-game (game-invaders G0) (game-missiles G0) (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (handle-keys G0 "right") (make-game (game-invaders G0) (game-missiles G0) (make-tank (tank-x (game-tank G0)) 1)))
(check-expect (handle-keys G0 " ") (make-game (game-invaders G0) (cons (make-missile (tank-x (game-tank G0)) TANK-Y)(game-missiles G0))
                                              (game-tank G0)))

;(define (handle-keys g ke) G0) ;stub

;<template according to KeyEvent>
(define (handle-keys g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g)(handle-tank (game-tank g) ke))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (handle-tank (game-tank g) ke))]
        [(key=? ke " ") (make-game (game-invaders g) (handle-space (game-missiles g)(game-tank g) ke) (game-tank g))]
        [else
         g]
    )
  )

;; Tank KeyEvent -> Tank
;; handle tank key presses, left arrow key moves tank left (dir -1), right arrow key moves tank right (dir 1), 
(check-expect (handle-tank T0 "up") T0)
(check-expect (handle-tank T0 "down") T0)
(check-expect (handle-tank T0 "left") (make-tank (tank-x T0) -1))
(check-expect (handle-tank T0 "right") (make-tank (tank-x T0) 1))

;(define (handle-tank t ke) T0) ;stub

;<template according to KeyEvent>
(define (handle-tank t ke)
  (cond [(key=? ke "left") (make-tank (tank-x t) -1)]
        [(key=? ke "right") (make-tank (tank-x t) 1)]
        [else
         t]
    )
  )

;; ListOfMissiles Tank -> ListOfMissiles
;; if space key is pressed, add a missile at tank x,y position
(check-expect (handle-space LOM0 T0 " ") (cons (make-missile (tank-x T0) TANK-Y) LOM0))
(check-expect (handle-space LOM0 T0 "up") LOM0)

;(define (handle-space lom t ke) LOM0) ;stub

;<template according to KeyEvent>
(define (handle-space lom t ke)
  (cond [(key=? ke " ") (cons (make-missile (tank-x t) TANK-Y) lom)]
        [else lom])
  )