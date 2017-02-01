;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket-scene) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Launching rocket program


;; Constants

(define HEIGHT 1200) ; distances in pixels
(define WIDTH (/ HEIGHT 3))

(define YDELTA (/ HEIGHT 400))

(define BACKG (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle (/ WIDTH 30) (/ HEIGHT 15) "solid" "red"))

(define ROCKET-X (/ WIDTH 2))
(define ROCKET-HALF-HEIGHT (/ (image-height ROCKET) 2))
(define ROCKET-INIT-Y (- HEIGHT ROCKET-HALF-HEIGHT))

(define COUNTDOWN-TEXT-SIZE 60)
(define COUNTDOWN-TEXT-COLOR "red")

;; Data definitions


;; LaunchingRocketCountdown is one of:
;;  - "resting"
;;  - Integer[-3, -1]
;;  - Natural
;; interp. status of launching rocket
;;  - "resting" is a grounded rocket
;;  - Integer[-3, -1] countdown to rocket launch
;;  - Natural is the distance between rocket bottom and the top of a canvas

#;
(define (fn-for-launching-rocket-countdown lrc)
  (cond
    [(string? lrc) (...)]
    [(<= -3 lrc -1) (... lrc)]
    [(<= 0 lrc) (... lrc)]))


;; Functions


;; LRC -> Image
;; renders the state as a resting or flying rocket
(check-expect
 (show "resting")
 (place-image ROCKET ROCKET-X ROCKET-INIT-Y BACKG))

(check-expect
 (show HEIGHT)
 (place-image ROCKET ROCKET-X (- 0 ROCKET-HALF-HEIGHT) BACKG))

(check-expect
 (show 0)
 (place-image ROCKET ROCKET-X ROCKET-INIT-Y BACKG))

(check-expect
 (show -2)
 (place-image (text "-2" COUNTDOWN-TEXT-SIZE COUNTDOWN-TEXT-COLOR)
              ROCKET-X (* 3/4 WIDTH)
              (place-image ROCKET ROCKET-X ROCKET-INIT-Y BACKG)))

(check-expect
 (show 10)
 (place-image ROCKET ROCKET-X (- HEIGHT 10 ROCKET-HALF-HEIGHT) BACKG))


(define (show lrc)
  (cond
    [(string? lrc)
     (draw-rocket ROCKET-INIT-Y)]
    [(<= -3 lrc -1)
     (place-image (text (number->string lrc)
                        COUNTDOWN-TEXT-SIZE
                        COUNTDOWN-TEXT-COLOR)
                  ROCKET-X (* 3/4 WIDTH)
                  (draw-rocket ROCKET-INIT-Y))]
    [(<= 0 lrc)
     (draw-rocket (- HEIGHT lrc ROCKET-HALF-HEIGHT))]))


;; Natural -> LRC
;; draws rocket at (ROCKET-X, y) on the BACKG
(check-expect (draw-rocket (/ HEIGHT 2))
              (place-image ROCKET ROCKET-X (/ HEIGHT 2) BACKG))

(define (draw-rocket y)
  (place-image ROCKET ROCKET-X y BACKG))


;; LRC KeyEvent -> LRC
;; starts the countdown when space bar is pressed, if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch lrc ke)
  (cond
    [(string? lrc) (if (string=? " " ke) -3 lrc)]
    [(<= -3 lrc -1) lrc]
    [(<= 0 lrc) lrc]))


;; LRC -> LRC
;; raises the rocket by YDELTA, if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 0) YDELTA)
(check-expect (fly 10) (+ 10 YDELTA))

(define (fly lrc)
  (cond
    [(string? lrc) lrc]
    [(<= -3 lrc -1) (if (= lrc -1) 0 (add1 lrc))]
    [(<= 0 lrc) (+ lrc YDELTA)]))


;; LRC -> Boolean
;; determines when we should stop displaying the scene (rocket goes out of sight)
(check-expect (rocket-invisible? 0) false)
(check-expect (rocket-invisible? HEIGHT) true)

(define (rocket-invisible? lrc)
  (and (number? lrc) (= HEIGHT lrc)))

;; LRC -> LRC
;; launches program; start with (main "resting")
(define (main lrc)
  (big-bang lrc
            [on-tick fly]
            [to-draw show]
            [on-key launch]
            [stop-when rocket-invisible?]))