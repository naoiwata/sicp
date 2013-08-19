;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.57.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

; --- use memo-proc
; Fib-Count(n) = n - 1
; θ(n) = n

; --- not use memo-proc
; Fib-Count(n) = Fib-Count(n-1) + Fib-Count(n-2) + n - 1
; θ(n) = φ^n

; END