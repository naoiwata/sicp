;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.38.
;;

; ------------------------------------------------------------------------
; (a) solution 
; ------------------------------------------------------------------------

; Peter: (set! balance (+ balance 10))
; Paul : (set! balance (- balance 20))
; Mary : (set! balance (- balance (/ balance 2)))

; Peter ->  Paul ->  Mary : 100 -> 110 -> 90 -> 45
; Peter ->  Mary ->  Paul : 100 -> 110 -> 55 -> 35
;  Paul -> Peter ->  Mary : 100 ->  80 -> 90 -> 45
;  Paul ->  Mary -> Peter : 100 ->  80 -> 40 -> 50
;  Mary -> Peter ->  Paul : 100 ->  50 -> 60 -> 40
;  Mary ->  Paul -> Peter : 100 ->  50 -> 30 -> 40
