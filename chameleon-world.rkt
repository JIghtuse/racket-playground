;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chameleon-world) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Chameleon walking across the screen with happiness gauge
;; (press down to feed; "r", "g", b" to change color)


;; Constants

(define WIDTH 1920)
(define HEIGHT 1080)
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define CHAM-IMG (bitmap "images/chameleon.png"))
(define CHAM-SPEED-X 3) ; in pixels per tick
(define HAPPINESS-GAUGE-COLOR-FG "red")
(define MAX-HAPPINESS 100)
(define HAPPINESS-DECREASE-RATE 0.1) ; happiness points per tick
(define HAPPINESS-FEED-INCREASE 2)   ; happiness points
(define GAUGE-WIDTH 30)
(define GAUGE-HEIGHT (* 3 MAX-HAPPINESS))
(define GAUGE-FRAME (rectangle GAUGE-WIDTH GAUGE-HEIGHT "outline" "black"))
(define GAUGE-X (- WIDTH 10 GAUGE-WIDTH))
(define GAUGE-Y (+ (/ GAUGE-HEIGHT 2) 10))


;; Data definitions

(define-struct vcham [x color happiness])
;; VCham is (make-vcham Natural[0..WIDTH] Color Natural[0..100])
;; interp. chameleon with:
;;           coordinate (x, CTR-Y) (center in screen coordinates)
;;           color
;;           happiness level

(define VC0 (make-vcham 0 "red" 100))
(define VC1 (make-vcham 22 "green" 10))

;; Functions


;; VCham -> VCham
;; launches the program from some initial state; start with (main VC0)
(define (main vc)
  (big-bang vc
            [on-tick tick]
            [to-draw draw]
            [on-key cham-reaction]
            [stop-when unhappy?]))


;; VCham -> Image
;; displays chameleon image on empty scene at coordinate (x, CTR-Y) with happiness gauge
(check-expect (draw (make-vcham 20 "red" 100))
              (place-image
               (draw-happiness-gauge 100)
               GAUGE-X GAUGE-Y
               (place-image (color-cham CHAM-IMG "red") 20 CTR-Y MTS)))

(check-expect (draw (make-vcham 10 "blue" 90))
              (place-image
               (draw-happiness-gauge 90)
               GAUGE-X GAUGE-Y
               (place-image (color-cham CHAM-IMG "blue") 10 CTR-Y MTS)))

(define (draw vc)
  (place-image
   (draw-happiness-gauge (vcham-happiness vc))
   GAUGE-X GAUGE-Y
   (place-image
    (color-cham CHAM-IMG (vcham-color vc))
    (vcham-x vc) CTR-Y MTS)))


;; Image Color -> Image
;; produces image overlayed with background of a given color
(check-expect
 (color-cham CHAM-IMG "red")
 (overlay CHAM-IMG (rectangle (image-width CHAM-IMG)
                              (image-height CHAM-IMG)
                              "solid"
                              "red")))
(check-expect
 (color-cham CHAM-IMG "blue")
 (overlay CHAM-IMG (rectangle (image-width CHAM-IMG)
                              (image-height CHAM-IMG)
                              "solid"
                              "blue")))

;(define (color-cham img c) img) ;stub
(define (color-cham img c)
  (overlay img (rectangle (image-width img) (image-height img) "solid" c)))


;; Integer -> Integer
;; given happiness level, produces number of pixels to fill on happiness gauge
(check-expect (happiness->height 100) GAUGE-HEIGHT)
(check-expect (happiness->height 0) 0)
(check-expect (happiness->height 30)
              (* GAUGE-HEIGHT (/ 30 MAX-HAPPINESS)))

;(define (happiness->height h) h) ;stub
(define (happiness->height h)
  (* GAUGE-HEIGHT (/ h MAX-HAPPINESS)))


;; Integer -> Image
;; produces image of a happiness gauge
(check-expect
 (draw-happiness-gauge 100)
 (overlay/xy GAUGE-FRAME 0 (- GAUGE-HEIGHT (happiness->height 100))
             (rectangle GAUGE-WIDTH (happiness->height 100) "solid"
                        HAPPINESS-GAUGE-COLOR-FG)))

(check-expect
 (draw-happiness-gauge 30)
 (overlay/xy GAUGE-FRAME 0 (- GAUGE-HEIGHT (happiness->height 30))
             (rectangle GAUGE-WIDTH (happiness->height 30) "solid"
                        HAPPINESS-GAUGE-COLOR-FG)))

;(define (draw-happiness-gauge h) empty-image)
(define (draw-happiness-gauge h)
  (overlay/xy GAUGE-FRAME 0 (- GAUGE-HEIGHT (happiness->height h))
              (rectangle GAUGE-WIDTH (happiness->height h) "solid"
                         HAPPINESS-GAUGE-COLOR-FG)))
  

;; VCham -> VCham
;; add CHAM-SPEED-X to x position, decrease happiness; wraps over WIDTH
(check-expect (tick (make-vcham 0 "red" 100))
              (make-vcham CHAM-SPEED-X "red" (decrease-happiness 100)))

(check-expect (tick (make-vcham (- WIDTH CHAM-SPEED-X) "green" 20))
              (make-vcham 0 "green" (decrease-happiness 20)))

(check-expect (tick (make-vcham (- WIDTH 1) "blue" 20))
              (make-vcham (modulo (+ CHAM-SPEED-X (- WIDTH 1)) WIDTH)
                          "blue"
                          (decrease-happiness 20)))

(check-expect (tick (make-vcham 7 "blue" 20))
              (make-vcham (+ 7 CHAM-SPEED-X) "blue" (decrease-happiness 20)))


(define (tick vc)
  (make-vcham
   (modulo (+ (vcham-x vc) CHAM-SPEED-X) WIDTH)
   (vcham-color vc)
   (decrease-happiness (vcham-happiness vc))))


;; Natural -> Natural
;; subtracts HAPPINESS-DECREASE-RATE from happiness, making values <= 0 zero
(check-expect (decrease-happiness 0) 0)
(check-expect (decrease-happiness -4) 0)
(check-expect (decrease-happiness 20) (- 20 HAPPINESS-DECREASE-RATE))
(define (decrease-happiness happiness)
  (max 0 (- happiness HAPPINESS-DECREASE-RATE)))


;; VCham KeyEvent -> VCham
;; Increase happiness by "feed increase" on key down, change color on "r", "g", "b"
;; do not increase values greater than MAX-HAPPINESS
(check-expect (cham-reaction (make-vcham 22 "green" 10) "down")
              (make-vcham 22 "green" (+ 10 HAPPINESS-FEED-INCREASE)))
(check-expect (cham-reaction (make-vcham 22 "blue" (sub1 MAX-HAPPINESS)) "down")
              (make-vcham 22 "blue" MAX-HAPPINESS))
(check-expect (cham-reaction (make-vcham 22 "blue" 20) "r")
              (make-vcham 22 "red" 20))
(check-expect (cham-reaction (make-vcham 22 "red" 20) "r")
              (make-vcham 22 "red" 20))
(check-expect (cham-reaction (make-vcham 22 "red" 20) "g")
              (make-vcham 22 "green" 20))
(check-expect (cham-reaction (make-vcham 22 "red" 20) "b")
              (make-vcham 22 "blue" 20))
(check-expect (cham-reaction VC1 "a")
              VC1)

;(define (cham-reaction vc ke) vc) ;stub
(define (cham-reaction vc ke)
  (cond
    [(key=? "down" ke)
     (make-vcham (vcham-x vc)
                 (vcham-color vc)
                 (min MAX-HAPPINESS
                      (+ HAPPINESS-FEED-INCREASE (vcham-happiness vc))))]
    [(key=? "r" ke)
     (make-vcham (vcham-x vc)
                 "red"
                 (vcham-happiness vc))]
    [(key=? "b" ke)
     (make-vcham (vcham-x vc)
                 "blue"
                 (vcham-happiness vc))]
    [(key=? "g" ke)
     (make-vcham (vcham-x vc)
                 "green"
                 (vcham-happiness vc))]
    [else vc]))

;; VCham -> Boolean
;; produces true when chameleon is totally not happy
(check-expect (unhappy? (make-vcham 10 "green" 2)) false)
(check-expect (unhappy? (make-vcham 10 "green" 0)) true)

;(define (unhappy? vc) false) ;stub

(define (unhappy? vc)
  (= 0 (vcham-happiness vc)))