(require 2htdp/image)
(require 2htdp/universe)


(define-struct editor [pre post])

(define scene (empty-scene 200 20))
(define cursor (rectangle 1 20 "solid" "red"))


(define (render e) (overlay (beside
                        (text/font (editor-pre e)
                                   16 "black" "default" "default" "normal" "normal" "default")
                        cursor (text/font
                                (editor-post e)
                                16 "black" "default" "default" "normal" "normal" "default")) scene))

(check-expect (render (make-editor "a" "b"))
              (overlay (beside
                        (text/font
                         "a" 16 "black" "default" "default" "normal" "normal" "default")
                        cursor (text/font
                                "b" 16 "black" "default" "default" "normal" "normal" "default"))
                       scene))

; ; A KeyEvent is one of: 
; – 1String
; – "left"
; – "right"
; – "up"
; – ...

; edit: Editor KeyEvent -> Editor
; consumes an editor and a keyevent and outputs another editor
; (define (edit e keyevent)...)


; string-rest: String -> String
; takes first character off a string and outputs the result
(define (string-rest s)
  (if (> (string-length s) 0)
      (substring s 1 (string-length s))
      ""))

; string-last-remove: String -> String
; removes last character of input string and outputs the resulting string

(define (string-last-remove s)
  (if (> (string-length s) 0)
      (substring s 0 (- (string-length s) 1))
      ""))


; String -> String
; outputs first character of string
(define (string-first s)
  (if (> (string-length s) 0)
      (substring s 0 1)
      ""))


; string-last: String -> String
; outputs last character of string
(define (string-last s)
  (if (> (string-length s) 0)
      (substring s (- (string-length s) 1) (string-length s))
      ""))



(define (edit ed ke)
  (cond
    [(key=? ke "\b")(make-editor (string-last-remove (editor-pre ed)) (editor-post ed))]
    [(key=? ke "left")
    (make-editor (string-last-remove (editor-pre ed))
                 (string-append (string-last (editor-pre ed)) (editor-post ed)))]
    [(key=? ke "right")
    (make-editor (string-append (editor-pre ed) (string-first (editor-post ed)))
                 (string-rest (editor-post ed)))]
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(= (string-length ke) 1) (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]
    [else                ed]))


(check-expect (edit (make-editor "a" "b") "c") (make-editor (string-append "a" "c") "b"))
(check-expect (edit (make-editor "a" "b") "\b") (make-editor "" "b")) 
(check-expect (edit (make-editor "a" "b") "\t") (make-editor "a" "b"))
(check-expect (edit (make-editor "a" "b") "\r") (make-editor "a" "b"))
(check-expect (edit (make-editor "a" "b") " ") (make-editor "a " "b"))
(check-expect (edit (make-editor "a" "b") "right") (make-editor "ab" ""))
(check-expect (edit (make-editor "a" "b") "left") (make-editor "" "ab"))



; run: String -> big-bang Animation
; given a pre field of an editor it launches an interactive editor

(define (run pre_field)
	(big-bang (make-editor pre_field "")
  	(to-draw render)
  	(on-key  edit)))

(run "")
