;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.58.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

;       num |  1   3   2   6   4
;       den |  7   7   7   7   7
;     radix | 10  10  10  10  10 
;  quotient |  1   4   2   8   5
; remainder |  3   2   6   4   5
;  *result* |  1   4   2   8   5 -> 1.4285 

; 10/7 = 1.4285714285714286

; END