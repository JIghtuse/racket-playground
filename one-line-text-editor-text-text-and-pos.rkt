;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname one-line-text-editor-text-text-and-pos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct editor [text cursor-pos])
;; Editor is (make-editor String Integer[0..IGNORE-INPUT-THRESHOLD + 1])
;; interp. one-line editor whose visible text is text
;; with the cursor displayed before cursor-pos letter


(define E0 (make-editor "" 0)) ; empty
(define E1 (make-editor "Helloworld" 5))
(define E2 (make-editor "bcdaabbcfkdjjdfjdfdfjjdfjdfjdfjjdfjdfjdfaabbccde"
                        IGNORE-INPUT-THRESHOLD))
; full
(define E3 (make-editor "bcdaabbcfkdjjdfjdfdfjjdfjdfjdfjjdfjdfjdfaabbccded"
                        IGNORE-INPUT-THRESHOLD))

#;
(define (fn-for-editor e)
  (... (editor-text e)
       (editor-cursor-pos e)))

;; Functions

;; Editor -> Editor
;; starts editor; run with (main (make-editor "" 0))
(define (main e)
  (big-bang e
            [to-draw render-editor]
            [on-key edit]))

;; String -> Editor
;; launches editor with a given string as pre field; for example, (run "hello")
(define (run s)
  (big-bang (make-editor s (string-length s))
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
   (beside (render-text (substring (editor-text e) 0 (editor-cursor-pos e)))
           CURSOR
           (render-text (substring (editor-text e) (editor-cursor-pos e))))
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
(check-expect (edit E0 " ") (make-editor " " 1))
(check-expect (edit E0 "z") (make-editor "z" 1))
(check-expect (edit E0 "k") (make-editor "k" 1))
(check-expect (edit E1 " ") (make-editor "Hello world" 6))
(check-expect (edit E1 "z") (make-editor "Hellozworld" 6))
(check-expect (edit E1 "k") (make-editor "Hellokworld" 6))

; deleting
(check-expect (edit E0 "\b") E0)
(check-expect (edit E1 "\b") (make-editor "Hellworld" 4))
(check-expect (edit (make-editor "oz" 0) "\b")
              (make-editor "oz" 0))
(check-expect (edit (make-editor "oz" 2) "\b")
              (make-editor "o" 1))

; moving
(check-expect (edit E0 "left") E0)
(check-expect (edit E0 "right") E0)
(check-expect (edit E1 "left") (make-editor "Helloworld" 4))
(check-expect (edit E1 "right") (make-editor "Helloworld" 6))

;; almost full - only one character to insert
(check-expect (edit E2 "z")
              (make-editor (string-append (editor-text E2) "z")
                           (add1 (editor-cursor-pos E2))))

;; no place to insert text - ignore keys
(check-expect (edit E3 "z") E3)


;(define (edit e ke) e) ;stub

(define (edit e ke)
  (cond
    [(key=? "left" ke)
     (move-cursor-left e)]
    [(key=? "right" ke)
     (move-cursor-right e)]
    [(key=? "\b" ke)
     (editor-delete-character e)]
    [(> (string-length (editor-text e)) IGNORE-INPUT-THRESHOLD)
     e]
    [(or (key=? "\t" ke) (key=? "\r" ke))
     e]
    [(= (string-length ke) 1)
     (editor-insert-character e ke)]
    [else e]))

;; Editor -> Editor
;; moves editor cursor to the left if it is possible (not at the left end)
(check-expect (move-cursor-left E0) E0)
(check-expect (move-cursor-left (make-editor "fadsf" 0))
              (make-editor "fadsf" 0))
(check-expect (move-cursor-left (make-editor "fadsf" 3))
              (make-editor "fadsf" 2))

;(define (move-cursor-left e) e) ;stub
(define (move-cursor-left e)
  (if (= 0 (editor-cursor-pos e))
      e
      (make-editor (editor-text e)
                   (sub1 (editor-cursor-pos e)))))

;; Editor -> Editor
;; moves editor to the right if it is possible (not at the right end)
(check-expect (move-cursor-right E0) E0)
(check-expect (move-cursor-right (make-editor "abc" 3))
              (make-editor "abc" 3))
(check-expect (move-cursor-right (make-editor "abc" 2))
              (make-editor "abc" 3))

;(define (move-cursor-right e) e) ;stub
(define (move-cursor-right e)
  (if (= (string-length (editor-text e))
         (editor-cursor-pos e))
      e
      (make-editor (editor-text e)
                   (add1 (editor-cursor-pos e)))))


;; Editor 1String -> Editor
;; inserts character at cursor position and advances cursor to the right
(check-expect (editor-insert-character (make-editor "" 0) "a")
              (make-editor "a" 1))
(check-expect (editor-insert-character (make-editor "ab" 0) "c")
              (make-editor "cab" 1))

;(define (editor-insert-character e c) e) ;stub
(define (editor-insert-character e c)
  (make-editor
   (insert-character (editor-text e) c (editor-cursor-pos e))
   (add1 (editor-cursor-pos e))))


;; String 1String Integer -> String
;; inserts character at the given position of input string
(check-expect (insert-character "" "a" 0) "a")
(check-expect (insert-character "ab" "c" 0) "cab")
(check-expect (insert-character "ab" "c" 1) "acb")
(check-expect (insert-character "ab" "c" 2) "abc")

;(define (insert-character s c pos) s) ;stub
(define (insert-character s c pos)
  (string-append (substring s 0 pos)
                 c
                 (substring s pos)))

;; Editor -> Editor
;; deletes character before the cursor, if not on the left end of the string
(check-expect (editor-delete-character (make-editor "" 0))
              (make-editor "" 0))
(check-expect (editor-delete-character (make-editor "a" 1))
              (make-editor "" 0))
(check-expect (editor-delete-character (make-editor "cab" 1))
              (make-editor "ab" 0))

;(define (editor-delete-character e) e) ;stub

(define (editor-delete-character e)
  (if (= 0 (editor-cursor-pos e))
      e
      (make-editor
       (delete-character (editor-text e) (editor-cursor-pos e))
       (sub1 (editor-cursor-pos e)))))


;; String Integer -> String
;; deletes character at the given position of input string
;; REQUIRES: given integer is <= string length
(check-expect (delete-character "a" 1) "")
(check-expect (delete-character "cab" 1) "ab")
(check-expect (delete-character "acb" 2) "ab")
(check-expect (delete-character "abc" 3) "ab")
(check-expect (delete-character "Helloworld" 5) "Hellworld")

;(define (delete-character s pos) s) ;stub

(define (delete-character s pos)
  (string-append (substring s 0 (sub1 pos))
                 (substring s pos)))