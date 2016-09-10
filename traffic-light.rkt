;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ============
;; Constants

(define RADIUS 100)
(define SPACING 10)
(define WIDTH (+ (* 2 SPACING) (* 2 RADIUS)))
(define HEIGHT (+ (* 4 SPACING) (* RADIUS 6)))


(define CTR-X (/ WIDTH 2))
(define CTR-Y-RED (+ SPACING RADIUS))
(define CTR-Y-YELLOW (+ CTR-Y-RED SPACING (* RADIUS 2)))
(define CTR-Y-GREEN (+ CTR-Y-YELLOW SPACING (* RADIUS 2)))

(define MTS (empty-scene WIDTH HEIGHT "black"))

(define LIGHT-POSITIONS
  (list (make-posn CTR-X CTR-Y-RED)
        (make-posn CTR-X CTR-Y-YELLOW)
        (make-posn CTR-X CTR-Y-GREEN)))

(define RON (place-images
             (list (circle RADIUS "solid" "red")
                   (circle RADIUS "outline" "yellow")
                   (circle RADIUS "outline" "green"))
             LIGHT-POSITIONS
             MTS))

(define YON (place-images
             (list (circle RADIUS "outline" "red")
                   (circle RADIUS "solid" "yellow")
                   (circle RADIUS "outline" "green"))
             LIGHT-POSITIONS
             MTS))

(define GON (place-images
             (list (circle RADIUS "outline" "red")
                   (circle RADIUS "outline" "yellow")
                   (circle RADIUS "solid" "green"))
             LIGHT-POSITIONS
             MTS))

;; ===================
;; Data Definitions

;; LightState is one of:
;;  - 'red
;;  - 'yellow
;;  - 'green
;; interp. currently lit traffic light
;; <examples are redundant for enumerations>

#;
(define (fn-for-light-state ls)
  (cond [(symbol=? ls 'red) (...)]
        [(symbol=? ls 'yellow) (...)]
        [(symbol=? ls 'green) (...)]))

;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: 'red
;;  - atomic distinct: 'yellow
;;  - atomic distinct: 'green

;; =================
;; Functions:

;; LightState -> LightState
;; start the world with (main 'red)
;; 
(define (main ls)
  (big-bang ls                       ; LightState
            (on-tick next-light 1)   ; LightState -> LightState
            (to-draw render-light))) ; LightState -> Image

;; LightState -> LightState
;; produce the next traffic light state
;(define (next-light ls) ls) ; stub

(check-expect (next-light 'red) 'green)
(check-expect (next-light 'green) 'yellow)
(check-expect (next-light 'yellow) 'red)

; <use template from LightState>
(define (next-light ls)
  (cond [(symbol=? ls 'red) 'green]
        [(symbol=? ls 'yellow) 'red]
        [(symbol=? ls 'green) 'yellow]))


;; LightState -> Image
;; render traffic light

;(define (render-light ls) empty-image) ;stub

(check-expect (render-light 'red) RON)
(check-expect (render-light 'yellow) YON)
(check-expect (render-light 'green) GON)

(define (render-light ls)
  (cond [(symbol=? ls 'red) RON]
        [(symbol=? ls 'yellow) YON]
        [(symbol=? ls 'green) GON]))