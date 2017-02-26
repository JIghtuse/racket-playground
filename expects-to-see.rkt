;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname expects-to-see) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Sequence of KeyEvents pattern recognition (aka regex matching)
(require 2htdp/image)
(require 2htdp/universe)


;; Constants

(define SQUARE-SIDE 400)

;; FSM states
(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

;; Data definitions

;; ExpectsToSee is one of:
;;  - AA
;;  - BB
;;  - DD
;;  - ER
;; interp. FSM to match regular expression "a (b|c)* d"

(define ETS0 AA)


;; Functions

;; ETS -> Image
;; produce image of current state (AA - white, BB - yellow, ER - red, DD - green)
(check-expect (draw-state AA)
              (square SQUARE-SIDE "solid" "white"))
(check-expect (draw-state BB)
              (square SQUARE-SIDE "solid" "yellow"))
(check-expect (draw-state ER)
              (square SQUARE-SIDE "solid" "red"))
(check-expect (draw-state DD)
              (square SQUARE-SIDE "solid" "green"))

(define (draw-state ets)
  (square SQUARE-SIDE "solid"
          (cond
            [(string=? ets AA) "white"]
            [(string=? ets BB) "yellow"]
            [(string=? ets ER) "red"]
            [(string=? ets DD) "green"])))

;; ETS KeyEvent -> ETS
;; performs transition to new FSM state
(check-expect (switch-state AA "a") BB)
(check-expect (switch-state AA "z") ER)
(check-expect (switch-state BB "k") ER)
(check-expect (switch-state BB "b") BB)
(check-expect (switch-state BB "c") BB)
(check-expect (switch-state BB "d") DD)

(define (switch-state ets ke)
  (cond
    [(and (key=? "a" ke) (string=? AA ets))
     BB]
    [(and (or (key=? "b" ke)
              (key=? "c" ke))
          (string=? BB ets))
     BB]
    [(and (key=? "d" ke) (string=? BB ets))
     DD]
    [else
     ER]))


;; ETS -> Boolean
;; produce true if we reached final state (DD or ER)
(check-expect (final-state? AA) false)
(check-expect (final-state? BB) false)
(check-expect (final-state? ER) true)
(check-expect (final-state? DD) true)

(define (final-state? ets)
  (or (string=? ER ets) (string=? DD ets)))

;; ETS -> ETS
;; starts program; launch with (main ETS0)
(define (main ets)
  (big-bang ets
            [to-draw draw-state]
            [on-key switch-state]
            [stop-when final-state? draw-state]))