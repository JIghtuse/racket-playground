;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cham-and-cat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



;; Constants

(define WIDTH 1920)
(define HEIGHT 1080)
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define CAT-IMAGE (bitmap "images/cat.png"))
(define CHAM-IMG (bitmap "images/chameleon.png"))
(define CHAM-SPEED-X 3) ; in pixels per tick
(define HAPPINESS-GAUGE-COLOR-FG "red")
(define MAX-HAPPINESS 100)
(define HAPPINESS-DECREASE-RATE 0.1) ; happiness points per tick
(define HAPPINESS-PET-INCREASE 1)
(define HAPPINESS-FEED-INCREASE 2)   ; happiness points
(define GAUGE-WIDTH 30)
(define GAUGE-HEIGHT (* 3 MAX-HAPPINESS))
(define GAUGE-FRAME (rectangle GAUGE-WIDTH GAUGE-HEIGHT "outline" "black"))
(define GAUGE-Y (/ GAUGE-HEIGHT 2))



;; Data definitions


(define-struct vcat [x dx happiness])
;; VCat is (make-vcat Natural[0..WIDTH] Integer Natural[0..100])
;; interp. cat with:
;;          coordinate (x, CTR-Y) (center in screen coordinates)
;;          velocity dx (in pixels per tick)
;;          happiness level

(define VCAT0 (make-vcat (- WIDTH 100) 10 100))
(define VCAT1 (make-vcat 22 -5 10))
(define VCAT2 (make-vcat 20 -3 100))
(define VCAT3 (make-vcat 20 -3 0))


(define-struct vcham [x color happiness])
;; VCham is (make-vcham Natural[0..WIDTH] Color Natural[0..100])
;; interp. chameleon with:
;;           coordinate (x, CTR-Y) (center in screen coordinates)
;;           color
;;           happiness level

(define VCHAM0 (make-vcham 0 "red" 100))
(define VCHAM1 (make-vcham 22 "green" 10))
(define VCHAM2 (make-vcham 90 "red" 100))
(define VCHAM3 (make-vcham 90 "red" 0))

(define-struct zoo [cat cham])
;; Zoo is (make-zoo VCat VCham)
;; interp. pair of cat and chameleon

(define ZOO0 (make-zoo VCAT0 VCHAM0))


;; Functions


;; Zoo -> Image
;; displays animals on empty scene on appropriate coordinates
(check-expect (draw (make-zoo VCAT2 VCHAM2))
              (place-cat VCAT2
                         (place-cham VCHAM2
                                     MTS)))

                    
(check-expect (draw (make-zoo VCAT0 VCHAM0))
              (place-cat VCAT0
                         (place-cham VCHAM0
                                     MTS)))

(define (draw z)
  (place-cat (zoo-cat z)
             (place-cham (zoo-cham z)
                         MTS)))

;; VCat Image -> Image
;; displays appropriate cat image on given scene at coordinate (x, CTR-Y)
(check-expect (place-cat (make-vcat 20 -3 100) MTS)
              (place-image
               (draw-happiness-gauge 100)
               20 GAUGE-Y
               (place-image CAT-IMAGE 20 CTR-Y MTS)))

(check-expect (place-cat (make-vcat 10 5 90) MTS)
              (place-image
               (draw-happiness-gauge 90)
               10 GAUGE-Y
               (place-image CAT-IMAGE 10 CTR-Y MTS)))

(define (place-cat vc scene)
  (place-image
   (draw-happiness-gauge (vcat-happiness vc))
   (vcat-x vc) GAUGE-Y
   (place-image
    CAT-IMAGE
    (vcat-x vc) CTR-Y scene)))



;; VCham Image -> Image
;; displays chameleon image on given scene at coordinate (x, CTR-Y) with happiness gauge
(check-expect (place-cham (make-vcham 20 "red" 100) MTS)
              (place-image
               (draw-happiness-gauge 100)
               20 GAUGE-Y
               (place-image (color-cham CHAM-IMG "red") 20 CTR-Y MTS)))

(check-expect (place-cham (make-vcham 10 "blue" 90) MTS)
              (place-image
               (draw-happiness-gauge 90)
               10 GAUGE-Y
               (place-image (color-cham CHAM-IMG "blue") 10 CTR-Y MTS)))

(define (place-cham vc scene)
  (place-image
   (draw-happiness-gauge (vcham-happiness vc))
   (vcham-x vc) GAUGE-Y
   (place-image
    (color-cham CHAM-IMG (vcham-color vc))
    (vcham-x vc) CTR-Y scene)))



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



;; Zoo -> Zoo
;; advances animals to next x position, decrease happiness; wraps over WIDTH
(check-expect (tick (make-zoo VCAT0 VCHAM0))
              (make-zoo
               (next-cat VCAT0)
               (next-cham VCHAM0)))

(check-expect (tick (make-zoo VCAT2 VCHAM2))
              (make-zoo
               (next-cat VCAT2)
               (next-cham VCHAM2)))


(define (tick z)
  (make-zoo
   (next-cat (zoo-cat z))
   (next-cham (zoo-cham z))))


;; Natural -> Natural
;; subtracts HAPPINESS-DECREASE-RATE from happiness, making values <= 0 zero
(check-expect (decrease-happiness 0) 0)
(check-expect (decrease-happiness -4) 0)
(check-expect (decrease-happiness 20) (- 20 HAPPINESS-DECREASE-RATE))
(define (decrease-happiness happiness)
  (max 0 (- happiness HAPPINESS-DECREASE-RATE)))


;; VCham -> VCham
;; add CHAM-SPEED-X to x position, decrease happiness; wraps over WIDTH
(check-expect (next-cham (make-vcham 0 "red" 100))
              (make-vcham CHAM-SPEED-X "red" (decrease-happiness 100)))

(check-expect (next-cham (make-vcham (- WIDTH CHAM-SPEED-X) "green" 20))
              (make-vcham 0 "green" (decrease-happiness 20)))

(check-expect (next-cham (make-vcham (- WIDTH 1) "blue" 20))
              (make-vcham (modulo (+ CHAM-SPEED-X (- WIDTH 1)) WIDTH)
                          "blue"
                          (decrease-happiness 20)))

(check-expect (next-cham (make-vcham 7 "blue" 20))
              (make-vcham (+ 7 CHAM-SPEED-X) "blue" (decrease-happiness 20)))


(define (next-cham vc)
  (make-vcham
   (modulo (+ (vcham-x vc) CHAM-SPEED-X) WIDTH)
   (vcham-color vc)
   (decrease-happiness (vcham-happiness vc))))



;; VCat -> VCat
;; add dx to x position, decrease happiness; negates dx on borders
(check-expect (next-cat (make-vcat 0 5 100))
              (make-vcat 5 5 (decrease-happiness 100)))

(check-expect (next-cat (make-vcat 23 -3 50))
              (make-vcat (+ 23 -3) -3 (decrease-happiness 50)))

(check-expect (next-cat (make-vcat (- WIDTH 5) 5 20))
              (make-vcat WIDTH 5 (decrease-happiness 20)))

(check-expect (next-cat (make-vcat (- WIDTH 3) 5 20))
              (make-vcat (- WIDTH 3) -5 20))

(check-expect (next-cat (make-vcat 7 -7 20))
              (make-vcat 0 -7 (decrease-happiness 20)))

(check-expect (next-cat (make-vcat 3 -7 20))
              (make-vcat 3 7 20))


(define (next-cat vc)
  (if (<= 0 (+ (vcat-dx vc) (vcat-x vc)) WIDTH)
      (make-vcat
       (+ (vcat-x vc) (vcat-dx vc))
       (vcat-dx vc)
       (decrease-happiness (vcat-happiness vc)))
      (make-vcat
       (vcat-x vc)
       (- 0 (vcat-dx vc))
       (vcat-happiness vc))))




;; Zoo KeyEvent -> Zoo
;; Increase animal happiness by pet or feed (up, down) or changes its color (r, g, b)
;; do not make happiness greater than MAX-HAPPINESS

(check-expect (zoo-reaction (make-zoo VCAT0 VCHAM0) "up")
              (make-zoo (cat-reaction VCAT0 "up")
                        (cham-reaction VCHAM0 "up")))
(check-expect (zoo-reaction ZOO0 "z")
              ZOO0)

(define (zoo-reaction z ke)
  (make-zoo
   (cat-reaction (zoo-cat z) ke)
   (cham-reaction (zoo-cham z) ke)))


;; Key -> Color
;; produce color chameleon should have given input key (r g b)
(check-expect (key->color "r") "red")
(check-expect (key->color "g") "green")
(check-expect (key->color "b") "blue")

(define (key->color ke)
  (cond
    [(key=? "r" ke) "red"]
    [(key=? "g" ke) "green"]
    [(key=? "b" ke) "blue"]))


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
(check-expect (cham-reaction VCHAM1 "a")
              VCHAM1)

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


;; VCat KeyEvent -> VCat
;; Increase happiness by "pet increase" on key up, by "feed increase" on key down
;; do not increase values greater than MAX-HAPPINESS
(check-expect (cat-reaction (make-vcat 30 5 40) "up")
              (make-vcat 30 5 (+ 40 HAPPINESS-PET-INCREASE)))
(check-expect (cat-reaction (make-vcat 22 -4 10) "down")
              (make-vcat 22 -4 (+ 10 HAPPINESS-FEED-INCREASE)))
(check-expect (cat-reaction (make-vcat 22 -4 (sub1 MAX-HAPPINESS)) "down")
              (make-vcat 22 -4 MAX-HAPPINESS))
(check-expect (cat-reaction VCAT1 "a")
              VCAT1)

;(define (cat-reaction vc ke) vc) ;stub
(define (cat-reaction vc ke)
  (make-vcat
   (vcat-x vc)
   (vcat-dx vc)
   (min MAX-HAPPINESS
        (+ (vcat-happiness vc)
           (cond
             [(key=? "up" ke) HAPPINESS-PET-INCREASE]
             [(key=? "down" ke) HAPPINESS-FEED-INCREASE]
             [else 0])))))


;; Zoo -> Boolean
;; produces true when animal is totally not happy
(check-expect (unhappy? (make-zoo VCAT0 VCHAM0)) false)
(check-expect (unhappy? (make-zoo VCAT0 VCHAM3)) true)
(check-expect (unhappy? (make-zoo VCAT3 VCHAM0)) true)
(check-expect (unhappy? (make-zoo VCAT3 VCHAM3)) true)

(define (unhappy? z)
  (or
   (= 0 (vcat-happiness (zoo-cat z)))
   (= 0 (vcham-happiness (zoo-cham z)))))



;; Zoo -> Zoo
;; launches the program from some initial state;
;; start with (cham-and-cat ZOO0)
(define (cham-and-cat z)
  (big-bang z
            [on-tick tick]
            [to-draw draw]
            [on-key zoo-reaction]
            [stop-when unhappy?]))