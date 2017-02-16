;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname text-editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; One-line text editor

;; Constants

(define FONT-SIZE 64)
(define FONT-COLOR "black")
(define FONT-FACE "Monospace")
(define FONT-FAMILY 'modern)

(define BACKGROUND-COLOR "white")
(define WIDTH (* 30 FONT-SIZE))
(define HEIGHT (* 1.5 FONT-SIZE))
(define MTS (empty-scene WIDTH HEIGHT BACKGROUND-COLOR))

(define CURSOR-COLOR "red")
(define CURSOR-WIDTH (max 2 (/ FONT-SIZE 20)))
(define CURSOR-HEIGHT FONT-SIZE)
(define CURSOR (rectangle CURSOR-WIDTH CURSOR-HEIGHT "solid" "red"))

(define IGNORE-INPUT-THRESHOLD 48)



;; Data definitions

(define-struct editor [pre post])
;; Editor is (make-editor String String)
;; interp. one-line editor whose visible text is (string-append s t)
;; with the cursor displayed between s and t


(define E0 (make-editor "" "")) ; empty
(define E1 (make-editor "Hello" "world"))
; full
(define E2 (make-editor "bcdaabbcfkdjjdfjdfdfjjdfjdfjdfjjdfjdfjdfaabbccded" ""))

#;
(define (fn-for-editor e)
  (... (editor-pre e)
       (editor-post e)))

;; Functions

;; Editor -> Editor
;; starts editor; run with (main (make-editor "" ""))
(define (main e)
  (big-bang e
            [to-draw render-editor]
            [on-key edit]))

;; String -> Editor
;; launches editor with a given string as pre field; for example, (run "hello")
(define (run s)
  (big-bang (make-editor s "")
            [to-draw render-editor]
            [on-key edit]))

;; Editor -> Image
;; draws editor: left part of the text, cursor, right part of the text
(check-expect
 (render-editor E0)
 (overlay/align
  "left" "center"
  (beside (render-text "") CURSOR (render-text ""))
  MTS))

(check-expect
 (render-editor E1)
 (overlay/align
  "left" "center"
  (beside (render-text "Hello") CURSOR (render-text "world"))
  MTS))

;(define (render-editor e)) ;stub

(define (render-editor e)
  (overlay/align
   "left" "center"
   (beside (render-text (editor-pre e)) CURSOR (render-text (editor-post e)))
   MTS))


;; String -> Image
;; produces image of the specified text with FONT-SIZE, FONT-FAMILY and FONT-COLOR
(check-expect (render-text "")
              (text/font "" FONT-SIZE FONT-COLOR
                         FONT-FACE FONT-FAMILY 'normal 'normal #f))
(check-expect (render-text "Hi")
              (text/font "Hi" FONT-SIZE FONT-COLOR
                         FONT-FACE FONT-FAMILY 'normal 'normal #f))

;(define (render-text s) empty-image) ;stub
(define (render-text s)
  (text/font s FONT-SIZE FONT-COLOR
             FONT-FACE FONT-FAMILY 'normal 'normal #f))


;; Editor KeyEvent -> Editor
;; edits text inside editor

; these are ignored
(check-expect (edit E0 "\t") E0)
(check-expect (edit E0 "\r") E0)
(check-expect (edit E1 "\t") E1)
(check-expect (edit E1 "\r") E1)
(check-expect (edit E1 "up") E1)

; typing
(check-expect (edit E0 " ") (make-editor " " ""))
(check-expect (edit E0 "z") (make-editor "z" ""))
(check-expect (edit E0 "k") (make-editor "k" ""))
(check-expect (edit E1 " ") (make-editor "Hello " "world"))
(check-expect (edit E1 "z") (make-editor "Helloz" "world"))
(check-expect (edit E1 "k") (make-editor "Hellok" "world"))

; deleting
(check-expect (edit E0 "\b") E0)
(check-expect (edit E1 "\b") (make-editor "Hell" "world"))
(check-expect (edit (make-editor "" "oz") "\b")
              (make-editor "" "oz"))
(check-expect (edit (make-editor "oz" "") "\b")
              (make-editor "o" ""))

; moving
(check-expect (edit E0 "left") E0)
(check-expect (edit E0 "right") E0)
(check-expect (edit E1 "left") (make-editor "Hell" "oworld"))
(check-expect (edit E1 "right") (make-editor "Hellow" "orld"))

;; no place to insert text - ignore keys
(check-expect (edit E2 "z") E2)


;(define (edit e ke) e) ;stub

(define (edit e ke)
  (cond
    [(key=? "left" ke)
     (make-editor (string-remove-last (editor-pre e))
                  (string-append (string-last (editor-pre e)) (editor-post e)))]
    [(key=? "right" ke)
     (make-editor (string-append (editor-pre e) (string-first (editor-post e)))
                  (string-rest (editor-post e)))]
    [(key=? "\b" ke)
     (make-editor (string-remove-last (editor-pre e))
                  (editor-post e))]
    [(> (+ (string-length (editor-pre e))
           (string-length (editor-post e))) IGNORE-INPUT-THRESHOLD)
     e]
    [(or (key=? "\t" ke) (key=? "\r" ke))
     e]
    [(= (string-length ke) 1)
     (make-editor (string-append (editor-pre e) ke)
                  (editor-post e))]
    [else e]))


;; String -> String
;; produces string without the last letter, or empty string if string length < 2
(check-expect (string-remove-last "") "")
(check-expect (string-remove-last "a") "")
(check-expect (string-remove-last "bac") "ba")

;(define (string-remove-last s) s) ;stub
(define (string-remove-last s)
  (if (< (string-length s) 2)
      ""
      (substring s 0 (sub1 (string-length s)))))

;; String -> String
;; produces last letter of a string or empty string if string is empty
(check-expect (string-last "") "")
(check-expect (string-last "a") "a")
(check-expect (string-last "bac") "c")

;(define (string-last s) s) ; stub
(define (string-last s)
  (if (= (string-length s) 0)
      ""
      (substring s (sub1 (string-length s)))))

;; String -> String
;; produces first letter of a string or empty string if string is empty
(check-expect (string-first "") "")
(check-expect (string-first "a") "a")
(check-expect (string-first "bac") "b")

;(define (string-first s) s) ;stub
(define (string-first s)
  (if (= (string-length s) 0)
      ""
      (substring s 0 1)))


;; String -> String
;; produces string with first character dropped, or empty string if string length < 2
(check-expect (string-rest "") "")
(check-expect (string-rest "a") "")
(check-expect (string-rest "bac") "ac")

;(define (string-rest s) s) ;stub

(define (string-rest s)
  (if (< (string-length s) 2)
      ""
      (substring s 1)))
