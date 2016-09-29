;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spinning-bears-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; spinning-bears-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

;;;;PROBLEM:
;;;;
;;;;In this problem you will design another world program. In this program the changing 
;;;;information will be more complex - your type definitions will involve arbitrary 
;;;;sized data as well as the reference rule and compound data. But by doing your 
;;;;design in two phases you will be able to manage this complexity. As a whole, this problem 
;;;;will represent an excellent summary of the material covered so far in the course, and world 
;;;;programs in particular.
;;;;
;;;;This world is about spinning bears. The world will start with an empty screen. Clicking
;;;;anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
;;;;but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the 
;;;;screen, a new upright bear appears and starts spinning.
;;;;
;;;;So each bear has its own x and y position, as well as its angle of rotation. And there are an
;;;;arbitrary amount of bears.
;;;;
;;;;To start, design a world that has only one spinning bear. Initially, the world will start
;;;;with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
;;;;world will replace the old bear with a new bear at the new spot. You can do this part 
;;;;with only material up through compound. 
;;;;
;;;;Once this is working you should expand the program to include an arbitrary number of bears.
;;;;
;;;;Image of a bear located in "images/bear.png" file.


;; Spinning bears world

;; =============
;; Constants

(define WIDTH 1920)
(define HEIGHT 1080)
(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))

(define BEAR-IMAGE (bitmap "images/bear.png"))

(define ROTATION-SPEED 3)

;; ===================
;; Data Definitions

(define-struct bear (x y angle))
;; Bear is (make-bear Natural[0..WIDTH] Natural[0..HEIGHT] Natural[0..359])
;; interp. spinning bear at position x, y rotate by angle

(define BEAR-1 (make-bear CENTER-X CENTER-Y 0))
(define BEAR-2 (make-bear 0 0 90))

#;
(define (fn-for-bear b)
  (... (bear-x b)          ; Natural[0..WIDTH]
       (bear-y b)          ; Natural[0..HEIGHT]
       (bear-angle b)))    ; Natural[0..360]

;; Template rules used:
;;  - compound: 2 fields



;; ListOfBear is one of:
;;  - empty
;;  - (cons Bear ListOfBear)
;; interp. list of bears

(define LOB-1 empty)
(define LOB-2 (cons BEAR-1 empty))
(define LOB-3 (cons (make-bear 0 0 0)
                    (cons (make-bear CENTER-X CENTER-Y 90)
                          (cons (make-bear WIDTH HEIGHT 134)
                                empty))))


#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else (... (fn-for-bear (first lob))   ; Bear
                   (fn-for-lob (rest lob)))])) ; Natural recursion

;; Template rules used:
;;  - one-of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Bear ListOfBear)
;;  - reference: (first lob) is Bear
;;  - self-reference: (rest lob) is ListOfBear


;; =================
;; Functions:


;; ListOfBear -> ListOfBear
;; start the world with (main empty)
;; 
(define (main lob)
  (big-bang lob                    ; ListOfBear
            (on-tick rotate-bears) ; ListOfBear -> ListOfBear
            (to-draw render-bears) ; ListOfBear -> Image
            (on-mouse new-bear)))  ; ListOfBear Integer Integer MouseEvent -> ListOfBear  


;; ListOfBear -> ListOfBear
;; rotate all bears by ROTATION-SPEED angle counter-clockwise; wrap around 360
(check-expect (rotate-bears empty) empty) ;stub

(check-expect (rotate-bears (cons (make-bear 0 0 0) empty))
              (cons (rotate-bear (make-bear 0 0 0)) empty))

(check-expect (rotate-bears (cons (make-bear 0 0 0)
                                  (cons (make-bear 0 0 (- 360 (+ 1 ROTATION-SPEED)))
                                        (cons (make-bear 0 0 (- 360 ROTATION-SPEED))
                                              empty))))
              (cons (rotate-bear (make-bear 0 0 0))
                    (cons (rotate-bear (make-bear 0 0 (- 360 (+ 1 ROTATION-SPEED))))
                          (cons (rotate-bear (make-bear 0 0 (- 360 ROTATION-SPEED)))
                                empty))))

;(define (rotate-bears lob) lob) ; stub

(define (rotate-bears lob)
  (cond [(empty? lob) empty]
        [else (cons (rotate-bear (first lob))
                    (rotate-bears (rest lob)))]))


;; Bear -> Bear
;; rotate bear by ROTATION-SPEED angle counter-clockwise; wrap around 360
(check-expect (rotate-bear (make-bear 0 0 0))
              (make-bear 0 0 ROTATION-SPEED))

(check-expect (rotate-bear (make-bear 0 0 (- 360 (+ 1 ROTATION-SPEED))))
              (make-bear 0 0 (- 360 1)))

(check-expect (rotate-bear (make-bear 0 0 (- 360 ROTATION-SPEED)))
              (make-bear 0 0 0))


;(define (rotate-bear b) b) ; stub

(define (rotate-bear b)
  (make-bear (bear-x b)
             (bear-y b)
             (modulo (+ (bear-angle b) ROTATION-SPEED) 360)))


;; ListOfBear -> Image
;; render list of bears

(check-expect (render-bears empty) MTS)

(check-expect (render-bears (cons (make-bear 0 0 0) empty))
              (place-image BEAR-IMAGE 0 0
                           MTS))

(check-expect (render-bears (cons (make-bear 0 0 0)
                                  (cons (make-bear CENTER-X CENTER-Y 90)
                                        (cons (make-bear WIDTH HEIGHT 130)
                                              empty))))
              (place-image BEAR-IMAGE 0 0
                           (place-image (rotate 90 BEAR-IMAGE) CENTER-X CENTER-Y
                                        (place-image (rotate 130 BEAR-IMAGE) WIDTH HEIGHT
                                                     MTS))))

;(define (render-bears lob) empty-image) ; stub

(define (render-bears lob)
  (cond [(empty? lob) MTS]
        [else (place-image
               (rotate (bear-angle (first lob)) BEAR-IMAGE)
               (bear-x (first lob))
               (bear-y (first lob))
               (render-bears (rest lob)))]))


;; ListOfBear Integer Integer MouseEvent -> ListOfBear
;; adds new bear to clicked position on button-down; bear is appended to list of bears

(check-expect (new-bear empty CENTER-X CENTER-Y "button-down")
              (cons (make-bear CENTER-X CENTER-Y 0) empty))

(check-expect (new-bear (cons (make-bear 10 10 0) empty)
                        CENTER-X CENTER-Y "button-down")
              (cons (make-bear CENTER-X CENTER-Y 0)
                    (cons (make-bear 10 10 0)
                          empty)))

(check-expect (new-bear (cons (make-bear 10 10 0) empty)
                        CENTER-X CENTER-Y "button-up")
              (cons (make-bear 10 10 0)
                    empty))


; define (new-bear lob x y me) lob) ;stub

(define (new-bear lob x y me)
  (cond [(mouse=? me "button-down") (cons (make-bear x y 0) lob)]
        [else lob]))

(main empty)
