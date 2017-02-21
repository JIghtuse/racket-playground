;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invaders-v01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Space Invaders vol.1


;; Constants

(define WIDTH 1920)
(define HEIGHT 1080)
(define MTS (above
             (empty-scene WIDTH (/ HEIGHT 2) "lightblue")
             (empty-scene WIDTH (/ HEIGHT 2) "lightgreen")))

(define TANK-WIDTH (/ WIDTH 10))
(define TANK-HEIGHT (* TANK-WIDTH 1/6))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "brown"))
(define TANK-Y (- HEIGHT (/ TANK-HEIGHT 2)))

(define UFO-WIDTH (/ WIDTH 10))
(define UFO-RADIUS (/ UFO-WIDTH 6))
(define UFO-COLOR "orange")
(define UFO (overlay
             (circle UFO-RADIUS "solid" UFO-COLOR)
             (rectangle UFO-WIDTH (/ UFO-WIDTH 20) "solid" UFO-COLOR)))
(define UFO-LAND-Y (- HEIGHT (/ (image-height UFO) 2)))
(define UFO-LANDING-SPEED 3)
(define UFO-DRIFT-X 5)

(define MISSILE-SIDE (* 2 TANK-HEIGHT))
(define MISSILE-SPEED (* 2 UFO-LANDING-SPEED))
(define MISSILE (triangle MISSILE-SIDE "solid" "black"))
(define MISSILE-ACTION-SQUARE MISSILE-SIDE)
(define MISSILE-EXPLOSION (star MISSILE-ACTION-SQUARE "solid" "red"))

; sample scene
;(place-image MISSILE 300 200 (place-image TANK 350 TANK-Y (place-image UFO 100 100 MTS)))


;; Data definitions

;; UFO is (make-posn Natural[0, WIDTH] Natural[0, HEIGHT])
;; interp. UFO location (using top-down, left-to-right convention)

(define UFO0 (make-posn (/ WIDTH 2) 10))
(define UFO1 (make-posn 120 50))
(define UFO2 (make-posn 100 120))

(define-struct tank [x dx])
;; Tank is (make-tank Number[0, WIDTH], Integer)
;; interp. tank at position (x, TANK-Y) and velocity dx pixels/tick

(define TANK0 (make-tank (/ (image-width TANK) 2) 5))

;; Missile is (make-posn Natural[0, WIDTH] Natural[0, HEIGHT])
;; interp. missile location

(define MISSILE1 (make-posn 100 TANK-Y))

(define-struct aim [ufo tank])
;; Aim is (make-aim UFO Tank)
;; interp. state with UFO and tank only, tank trying to aim on UFO

(define-struct fired [ufo tank missile])
;; Fired is (make-fired UFO Tank Missile)
;; interp. state with launched missile from tank to UFO

;; SIGS is one of:
;;  - (make-aim UFO Tank)
;;  - (make-fired UFO Tank Missile)
;; interp. complete state of a space invader game

;; good state to start a game
(define SIGS0 (make-aim UFO0 TANK0))

;; tank maneuvering into position to fire the missile
(define SIGS1
  (make-aim (make-posn 20 10)
            (make-tank 28 5)))

;; missile has been fired
(define SIGS2
  (make-fired (make-posn 20 10)
              (make-tank 28 2)
              (make-posn 28 (- HEIGHT TANK-HEIGHT))))

;; missile is about to collide with the UFO
(define SIGS3
  (make-fired (make-posn 20 100)
              (make-tank 100 -3)
              (make-posn 22 103)))

;; UFO landed on a planet
(define SIGS4
  (make-aim (make-posn 100 UFO-LAND-Y)
            (make-tank 28 10)))

;; it was a miss - UFO landed
(define SIGS5
  (make-fired (make-posn 100 HEIGHT)
              (make-tank 150 -8)
              (make-posn 120 200)))



;; Functions

;; SIGS -> Sigs
;; Launches program; run with (si-main SIGS0)
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]))


;; SIGS -> Image
;; renders the given game state on top of MTS

(check-expect
 (si-render SIGS1)
 (place-image UFO 20 10
              (place-image TANK 28 TANK-Y MTS)))

(check-expect
 (si-render SIGS2)
 (place-image MISSILE 28 (- HEIGHT TANK-HEIGHT)
              (place-image UFO 20 10
                           (place-image TANK 28 TANK-Y MTS))))

(check-expect
 (si-render SIGS3)
 (place-image MISSILE 22 103
              (place-image UFO 20 100
                           (place-image TANK 100 TANK-Y MTS))))
              
(define (si-render s)
  (cond
    [(aim? s)
     (render-tank (aim-tank s)
                  (render-ufo (aim-ufo s) MTS))]
    [(fired? s)
     (render-missile (fired-missile s)
                     (render-tank (fired-tank s)
                                  (render-ufo (fired-ufo s) MTS)))]))


;; Tank Image -> Image
;; places tank image on given image
(check-expect (render-tank (make-tank 10 5) MTS)
              (place-image TANK 10 TANK-Y MTS))

(define (render-tank t bg)
  (place-image TANK (tank-x t) TANK-Y bg))

;; UFO Image -> Image
;; places ufo image on given background image
(check-expect (render-ufo (make-posn 50 100) MTS)
              (place-image UFO 50 100 MTS))

(define (render-ufo ufo bg)
  (place-image UFO (posn-x ufo) (posn-y ufo) bg))

;; Missile Image -> Image
;; places missile image on given background image
(check-expect (render-missile (make-posn 50 80) MTS)
              (place-image MISSILE 50 80 MTS))

(define (render-missile m bg)
  (place-image MISSILE (posn-x m) (posn-y m) bg))


;; SIGS -> Boolean
;; produces true if missile is collided with UFO or UFO is landed
(check-expect (si-game-over? SIGS5) true)

(check-expect
 (si-game-over?
  (make-aim (make-posn 100 (- HEIGHT 2))
            (make-posn 150 1)))
 true)

(check-expect
 (si-game-over?
  (make-aim (make-posn 100 10)
            (make-posn 150 9)))
 false)

(check-expect
 (si-game-over?
  (make-fired (make-posn 100 20)
              (make-tank 120 3)
              (make-posn 150 140)))
 false)

(check-expect (si-game-over? SIGS3) true)

(define (si-game-over? s)
  (cond
    [(aim? s)
     (ufo-landed? (posn-y (aim-ufo s)))]
    [(fired? s)
     (or (ufo-landed? (posn-y (fired-ufo s)))
         (<= (distance (fired-ufo s) (fired-missile s))
             MISSILE-ACTION-SQUARE))]))

;; Integer[0, HEIGHT] -> Boolean
;; produce true if y coordinate is greater or equal to UFO-LAND-Y
(check-expect (ufo-landed? 10) false)
(check-expect (ufo-landed? (sub1 UFO-LAND-Y)) false)
(check-expect (ufo-landed? UFO-LAND-Y) true)
(check-expect (ufo-landed? (add1 UFO-LAND-Y)) true)

(define (ufo-landed? y)
  (>= y UFO-LAND-Y))

;; Posn Posn -> Number
;; produce distance between two cartesian points
(check-expect (distance (make-posn 0 0) (make-posn 3 4)) 5)
(check-expect (distance (make-posn -3 4) (make-posn 0 0)) 5)

(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))


;; SIGS -> Image
;; produce image of a final game state
(check-expect
 (si-render-final SIGS3)
 (place-image MISSILE-EXPLOSION 22 103 (si-render SIGS3)))

(check-expect (si-render-final SIGS4) (si-render SIGS4))
(check-expect (si-render-final SIGS5) (si-render SIGS5))

(define (si-render-final s)
  (cond
    [(aim? s)
     (si-render s)]
    [(fired? s)
     (if (ufo-landed? (posn-y (fired-ufo s)))
         (si-render s)
         (place-image
          MISSILE-EXPLOSION
          (posn-x (fired-missile s)) (posn-y (fired-missile s))
          (si-render s)))]))


;; SIGS -> SIGS
;; produce next game state
(define (si-move s)
  (cond
    [(aim? s)
     (si-move-proper s (random-drift-x (posn-x (aim-ufo s))))]
    [(fired? s)
     (si-move-proper s (random-drift-x (posn-x (fired-ufo s))))]))


;; Natural[0..WIDTH] -> Natural[0..WIDTH]
;; produce random value within [x - UFO-DRIFT-X, x + UFO-DRIFT-X] from the given value x
(check-random (random-drift-x 5)
              (+ 5
                 (- UFO-DRIFT-X (random (* 2 UFO-DRIFT-X)))))

(define (random-drift-x x)
  (+ x
     (- UFO-DRIFT-X (random (* 2 UFO-DRIFT-X)))))

;; SIGS Number -> SIGS
;; move the space-invader objects predictably to (x, UFO-LANDING-SPEED)
(check-expect (si-move-proper
               (make-aim (make-posn 120 100)
                         (make-tank 128 5))
               123)
              (make-aim (make-posn 123 (+ 100 UFO-LANDING-SPEED))
                        (make-tank (+ 128 5) 5)))

(check-expect (si-move-proper
               (make-fired (make-posn 120 120)
                           (make-tank 108 5)
                           (make-posn 110 100))
               110)
              (make-fired (make-posn 110 (+ 120 UFO-LANDING-SPEED))
                          (make-tank (+ 108 5) 5)
                          (make-posn 110 (- 100 MISSILE-SPEED))))

(define (si-move-proper s x)
  (cond
    [(aim? s)
     (make-aim (advance-ufo (aim-ufo s) x)
               (advance-tank (aim-tank s)))]
    [(fired? s)
     (make-fired (advance-ufo (fired-ufo s) x)
                 (advance-tank (fired-tank s))
                 (advance-missile (fired-missile s)))]))


;; UFO Integer -> UFO
;; advances UFO to (x, current-y + UFO-LANDING-SPEED)
(check-expect (advance-ufo (make-posn 100 20) 85)
              (make-posn 85 (+ 20 UFO-LANDING-SPEED)))
(check-expect (advance-ufo (make-posn 90 40) 92)
              (make-posn 92 (+ 40 UFO-LANDING-SPEED)))

(define (advance-ufo u x)
  (make-posn x
             (+ (posn-y u) UFO-LANDING-SPEED)))

;; Tank -> Tank
;; advances tank by dx on x axis; bounces off edges
(check-expect (advance-tank (make-tank 10 5))
              (make-tank 15 5))
(check-expect (advance-tank (make-tank (- WIDTH 5) 5))
              (make-tank WIDTH 5))
(check-expect (advance-tank (make-tank (- WIDTH 2) 5))
              (make-tank (- WIDTH 2) -5))
(check-expect (advance-tank (make-tank 6 -6))
              (make-tank 0 -6))
(check-expect (advance-tank (make-tank 3 -6))
              (make-tank 3 6))
(check-expect (advance-tank (make-tank 10 -2))
              (make-tank 8 -2))

(define (advance-tank t)
  (if (<= 0 (+ (tank-x t) (tank-dx t)) WIDTH)
      (make-tank (+ (tank-x t) (tank-dx t))
                 (tank-dx t))
      (make-tank (tank-x t) (- 0 (tank-dx t)))))

;; Missile -> Missile
;; advances missile by MISSILE-SPEED up y axis
(check-expect (advance-missile (make-posn 30 40))
              (make-posn 30 (- 40 MISSILE-SPEED)))

(define (advance-missile m)
  (make-posn (posn-x m)
             (- (posn-y m) MISSILE-SPEED)))


;; SIGS KeyEvent -> SIGS
;; changes tank move direction on left/right and fires the missile on space bar
(check-expect (si-control SIGS0 " ")
              (make-fired (aim-ufo SIGS0)
                          (aim-tank SIGS0)
                          (make-posn (tank-x (aim-tank SIGS0)) TANK-Y)))

(check-expect (si-control SIGS2 " ") SIGS2)
(check-expect (si-control SIGS2 "a") SIGS2)
(check-expect (si-control SIGS0 "a") SIGS0)
(check-expect (si-control (make-aim UFO1 (make-tank 110 50)) "left")
              (make-aim UFO1 (make-tank 110 -50)))
(check-expect (si-control (make-aim UFO1 (make-tank 110 -50)) "left")
              (make-aim UFO1 (make-tank 110 -50)))
(check-expect (si-control (make-aim UFO2 (make-tank 100 -3)) "right")
              (make-aim UFO2 (make-tank 100 3)))
(check-expect (si-control (make-aim UFO2 (make-tank 100 3)) "right")
              (make-aim UFO2 (make-tank 100 3)))
(check-expect (si-control (make-fired UFO2 (make-tank 100 -3) MISSILE1) "left")
              (make-fired UFO2 (make-tank 100 -3) MISSILE1))
(check-expect (si-control (make-fired UFO2 (make-tank 100 3) MISSILE1) "left")
              (make-fired UFO2 (make-tank 100 -3) MISSILE1))
(check-expect (si-control (make-fired UFO2 (make-tank 100 -3) MISSILE1) "right")
              (make-fired UFO2 (make-tank 100 3) MISSILE1))
(check-expect (si-control (make-fired UFO2 (make-tank 100 3) MISSILE1) "right")
              (make-fired UFO2 (make-tank 100 3) MISSILE1))


(define (si-control s ke)
  (cond
    [(and (key=? ke " ") (aim? s))
     (make-fired (aim-ufo s)
                 (aim-tank s)
                 (make-posn (tank-x (aim-tank s)) TANK-Y))]
    [(key=? ke "left")
     (if (or (and (aim? s) (< 0 (tank-dx (aim-tank s))))
             (and (fired? s) (< 0 (tank-dx (fired-tank s)))))
         (reverse-tank-direction-world s)
         s)]
    [(key=? ke "right")
     (if (or (and (aim? s) (> 0 (tank-dx (aim-tank s))))
             (and (fired? s) (> 0 (tank-dx (fired-tank s)))))
         (reverse-tank-direction-world s)
         s)]
    [else s]))


;; SIGS -> SIGS
;; produce a world with tank moving in reversed direciton
(check-expect
 (reverse-tank-direction-world
  (make-aim UFO1 (make-tank 120 10)))
 (make-aim UFO1 (make-tank 120 -10)))

(check-expect
 (reverse-tank-direction-world
  (make-fired UFO2 (make-tank 10 20) MISSILE1))
 (make-fired UFO2 (make-tank 10 -20) MISSILE1))

(define (reverse-tank-direction-world s)
  (cond
    [(aim? s)
     (make-aim (aim-ufo s)
               (make-tank (tank-x (aim-tank s))
                          (- 0 (tank-dx (aim-tank s)))))]
    [(fired? s)
     (make-fired (fired-ufo s)
                 (make-tank (tank-x (fired-tank s))
                            (- 0 (tank-dx (fired-tank s))))
                 (fired-missile s))]))