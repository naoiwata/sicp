;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.36.
;;

; ------------------------------------------------------------------------
; question / test
; ------------------------------------------------------------------------

(add-load-path "." :relative)
(load "pages/3.3.5.scm")

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

; (for-each-except setter inform-about-value constraints)
; draw on a paper.

; END