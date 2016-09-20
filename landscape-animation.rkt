;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname landscape-animation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; TODO:
;;  - make move more smooth (by arc)
;;  - final color should be (250, 214, 165)


;; =================
;; Constants

(define WIDTH 1920)
(define HEIGHT 1080)
(define SUN-RADIUS 90)
(define SUN-COLOR "yellow")
(define MORNING-COLOR (make-color 100 100 255))
(define SPEED (/
               (/ (- WIDTH (* 2 SUN-RADIUS)) 2)
               100))

;; =================
;; Data Definitions


(define-struct sun (x y))
;; Sun is (make-sun Natural[0..WIDTH] Natural[0..HEIGHT])
;; interp. sun with specified screen coordinates (x, y)

(define SUN-1 (make-sun 0 (/ HEIGHT 2)))                    ;morning
(define SUN-2 (make-sun (- WIDTH SUN-RADIUS) (/ HEIGHT 2))) ;evening


#;
(define (fn-for-sun s)
  (... (sun-x s)       ; Natural[0..WIDTH]
       (sun-y s)))     ; Natural[0..HEIGHT]

;; Template rules used:
;;  - compound: 2 fields

(define-struct landscape (background-color sun))
;; Landscape is (make-landscape Color Sun)
;; interp. landscape with specified background color and sun
;;         background-color is the color of sky in the current time of day
;;         sun is a screen coordinates of a sun

(define LANDSCAPE-1 (make-landscape "blue"
                                    (make-sun (/ WIDTH 2)
                                              (- HEIGHT SUN-RADIUS))))

(define LANDSCAPE-2 (make-landscape "orange"
                                    (make-sun (- WIDTH SUN-RADIUS)
                                              (/ HEIGHT 2))))

#;
(define (fn-for-landscape ls)
  (... (landscape-background-color ls) ; Color
       (landscape-sun ls)))            ; Sun

;; Template rules used:
;;  - compound: 2 fields


;; =================
;; Functions:

;; Landscape -> Landscape
;; start the world with
#;
(main (make-landscape MORNING-COLOR
                      (make-sun SUN-RADIUS (/ HEIGHT 2))))

;; 
(define (main ls)
  (big-bang ls                            ; Landscape
            (on-tick next-landscape)      ; Landscape -> Landscape
            (stop-when night-time)        ; Landscape -> Boolean
            (to-draw render-landscape)))  ; Landscape -> Image



;; Color -> Color
;; produces next "morning" color (red - 1) (green - 1)
;(define (next-morning-color c) c) ;stub

(check-expect (next-morning-color (make-color 100 100 255))
              (make-color 99 99 255))

(check-expect (next-morning-color (make-color 1 1 255))
              (make-color 0 0 255))

#;
(define (next-morning-color c)
  (... c))

;; Template rules used:
;;  - Atomic non-distinct: Color

(define (next-morning-color c)
  (make-color (- (color-red c) 1)
              (- (color-green c) 1)
              (color-blue c)))



;; Color -> Color
;; produces next "evening" color (red + 2) (blue - 2)
;(define (next-evening-color c) c) ;stub

(check-expect (next-evening-color (make-color 0 0 255))
              (make-color 2 0 253))

(check-expect (next-evening-color (make-color 99 0 57))
              (make-color 101 0 55))

#;
(define (next-evening-color c)
  (... c))

;; Template rules used:
;;  - Atomic non-distinct: Color

(define (next-evening-color c)
  (make-color (+ (color-red c) 2)
              (color-green c)
              (- (color-blue c) 2)))



;; Sun -> Sun
;; produces next "morning" (rising) sun position (x + SPEED, y - SPEED/2)
;(define (next-morning-sun s) s) ;stub

(check-expect (next-morning-sun (make-sun 0 (/ SPEED 2)))
              (make-sun SPEED 0))

(define (next-morning-sun s)
  (make-sun (+ (sun-x s) SPEED)
            (- (sun-y s) (/ SPEED 2))))



;; Sun -> Sun
;; produces next "evening" (fading) sun position (x + SPEED, y + SPEED/2)
;(define (next-evening-sun s) s) ;stub

(check-expect (next-evening-sun (make-sun (- WIDTH SUN-RADIUS) 0))
              (make-sun (+ (- WIDTH SUN-RADIUS) SPEED)
                        (/ SPEED 2)))

(define (next-evening-sun s)
  (make-sun (+ (sun-x s) SPEED)
            (+ (sun-y s) (/ SPEED 2))))



;; Sun -> Boolean
;; produces true if it is morning and false otherwise
;(define (morning? s) false) ;stub
(check-expect (morning? (make-sun 0 0)) true)
(check-expect (morning? (make-sun (/ WIDTH 2) 0)) false)

#;
(define (morning? s)
  (sun-x s))

(define (morning? s)
  (< (sun-x s) (/ WIDTH 2)))


;; Landscape -> Landscape
;; move sun on the sky and change sky color (blue-white on morning, blue daily, orange on evening)

;(define (next-landscape ls) ls) ;stub

(check-expect (next-landscape
               (make-landscape MORNING-COLOR SUN-1))
              (make-landscape (next-morning-color MORNING-COLOR)
                              (next-morning-sun SUN-1)))

(check-expect (next-landscape
               (make-landscape (make-color 99 0 57) SUN-2))
              (make-landscape (next-evening-color (make-color 99 0 57))
                              (next-evening-sun SUN-2)))


(define (next-landscape ls)
  (if (morning? (landscape-sun ls))
      (make-landscape (next-morning-color (landscape-background-color ls))
                      (next-morning-sun (landscape-sun ls)))
      (make-landscape (next-evening-color (landscape-background-color ls))
                      (next-evening-sun (landscape-sun ls)))))



;; Landscape -> Image
;; render landscape on specified state
;(define (render-landscape ls) empty-image) ;stub


(check-expect (render-landscape
               (make-landscape (make-color 100 0 0)
                               (make-sun (- WIDTH SUN-RADIUS)
                                         (/ HEIGHT 2))))
              (place-image
               (circle SUN-RADIUS "solid" SUN-COLOR)
               (- WIDTH SUN-RADIUS)
               (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT (make-color 100 0 0))))


(check-expect (render-landscape
               (make-landscape (make-color 0 0 255)
                               (make-sun (/ WIDTH 2)
                                         SUN-RADIUS)))
              (place-image
               (circle SUN-RADIUS "solid" SUN-COLOR)
               (/ WIDTH 2)
               SUN-RADIUS
               (empty-scene WIDTH HEIGHT (make-color 0 0 255))))

(define (render-landscape ls)
  (place-image
   (circle SUN-RADIUS "solid" SUN-COLOR)
   (sun-x (landscape-sun ls))
   (sun-y (landscape-sun ls))
   (empty-scene WIDTH HEIGHT (landscape-background-color ls))))



;; Landscape -> Boolean
;; produces true when sun reached right end of screen (night falls)
;(define (night-time ls) false) ;stub

(check-expect (night-time LANDSCAPE-1) false)
(check-expect (night-time LANDSCAPE-2) true)

(define (night-time ls)
  (>= (sun-x (landscape-sun ls)) (- WIDTH SUN-RADIUS)))