;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;; start the world with (main G0)
;!!

(define (main g)
  (big-bang g
    (on-tick  next-game)
    (to-draw  render-game)
    (on-key   handle-keys)
    )
  )


;; Game -> Game
;; produce next state of game
;!!!

;(define (next-game g) T0) ;stub

(define (next-game g)
  (make-game empty (next-missiles (game-missiles g)) (next-tank (game-tank g)))
  )

;; Game -> Image
;; render game
;!!!

;(define (render-game g) BACKGROUND) ;stub

(define (render-game g)
  (render-missiles (game-missiles g)
                   (render-tank (game-tank g)))
  )

;; ListOfInvaders -> ListOfInvaders
;;produce filtered and ticked list of invaders

(define (tick-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (tick-invader (first loi))
               (tick-invaders (rest loi)))])
  )

;; Invader -> Invader
;; produce a new invader that is one pixel farther up the screen
(check-expect (tick-invader I1) (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 (* 12 INVADER-Y-SPEED)) 12))
(check-expect (tick-invader I2) (make-invader (+ 150 (* -10 INVADER-X-SPEED)) (+ HEIGHT (* 10 INVADER-Y-SPEED)) -10))

;<template from Invader>

(define (tick-invader i)
  (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) (+ (invader-y i) (* (abs (invader-dx i)) INVADER-Y-SPEED)) (invader-dx i))

  )

;; ListOfMissiles -> ListOfMissiles
;; produce filtered and ticked list of missiles

;<template as function composition>

(define (next-missiles lom)
  (onscreen-only (tick-missiles lom)))

;; ListOfMissiles -> ListOfMissiles
;; produce list of ticked missiles

;<template from ListOfMissiles>

(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else 
         (cons (tick-missile (first lom))
               (tick-missiles (rest lom)))]))

;; Missile -> Missile
;; produce a new missile that is one pixel farther up the screen

;<template from Missile>

(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissiles -> ListOfMissiles
;; produce a list containing only those missiles in lom that are onscreen?

;<template from ListOfMissiles>

(define (onscreen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (onscreen? (first lom))
             (cons (first lom) (onscreen-only (rest lom)))
             (onscreen-only (rest lom)))]))


;; Missile -> Boolean
;; produce true if m has not travelled past the top of BACKGROUND

;<template from Missile>


(define (onscreen? m)  
  (<= 0 (missile-y m)))


;; ListOfMissiles -> Image
;; Render the missiles onto BACKGROUND

;<template from ListOfMissiles w/ extra atomic parameter>

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else 
         (place-missile (first lom)
                        (render-missiles (rest lom) img))]))


;; Missiles Image -> Image
;; place missile on img as specified by m


;<template from Missile>

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))



;; Tank -> Tank
;; produce next tank, by advancing it TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
(check-expect (next-tank T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (next-tank T2) (make-tank (- 50 TANK-SPEED) -1))

(check-expect (next-tank (make-tank (- WIDTH TANK-SPEED) 1)) (make-tank WIDTH 1))
(check-expect (next-tank (make-tank TANK-SPEED -1)) (make-tank 0 -1))

(check-expect (next-tank (make-tank (- WIDTH 1) 1)) (make-tank WIDTH -1))
(check-expect (next-tank (make-tank 1 -1)) (make-tank 0 1))


;(define (next-tank t) T) ;stub

(define (next-tank t)
  (edge-tank(advance-tank t))
  )

;; Tank -> Tank
;; advance tank by TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
(check-expect (advance-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (advance-tank (make-tank (/ WIDTH 2) -1)) (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))

;(define (advance-tank t) T0) ;stub

;<use template from Tank>

(define (advance-tank t)
    (if (= (tank-dir t) 1)
      (make-tank (+ (tank-x t) TANK-SPEED) 1)
      (make-tank (- (tank-x t) TANK-SPEED) -1))) 

;; Tank -> Tank
;; when tank gets to edge, change dir to dir -1 if at right edge (WIDTH), dir 1 if at left edge (0)

;(define (edge-tank t) T0) ;stub

;<use template from Tank>
(define (edge-tank t)
  (cond [(> (tank-x t) WIDTH) (make-tank WIDTH -1)]
        [(< (tank-x t) 0) (make-tank 0 1)]
        [else
         (make-tank (tank-x t) (tank-dir t))])
  )


;; Tank -> Image
;; place appropriate tank image on BACKGROUND at (tank-x t) and HEIGHT - TANK-HEIGHT/2

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND)
  )

;; Game KeyEvent -> Game
;; handle key presses, left arrow key moves tank left (dir -1), right arrow key moves tank right (dir 1), 
;;                     space key creates missile at tank-x t and tank y position
;;check expects

;(define (handle-keys g ke) G0) ;stub

(define (handle-keys g ke)
  (cond [(key=? ke "left") (make-game empty (game-missiles g)(handle-tank (game-tank g) ke))]
        [(key=? ke "right") (make-game empty (game-missiles g) (handle-tank (game-tank g) ke))]
        [(key=? ke " ") (make-game empty (handle-space (game-missiles g)(game-tank g) ke) (game-tank g))]
        [else
         g]
    )
  )

;; Tank KeyEvent -> Tank
;; handle tank key presses, left arrow key moves tank left (dir -1), right arrow key moves tank right (dir 1), 

;;check expects

;(define (handle-tank t ke) T0) ;stub

(define (handle-tank t ke)
  (cond [(key=? ke "left") (make-tank (tank-x t) -1)]
        [(key=? ke "right") (make-tank (tank-x t) 1)]
        [else
         t]
    )
  )

;; ListOfMissiles Tank -> ListOfMissiles
;; if space key is pressed, add a missile at tank x,y position

;(define (handle-space lom t ke) LOM0) ;stub

(define (handle-space lom t ke)
  (cond [(key=? ke " ") (cons (make-missile (tank-x t) TANK-Y) lom)]
        [else lom])
  )