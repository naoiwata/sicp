;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.35
;;

; in fibonacci, x^2 - x - 1 = 0
; and x != 0
; so, x = 1 + 1/x
; when f(x) = x, then f(x) = x = 1 + 1/x
; x |-> 1 + 1/x has proven.

(add-load-path "." :relative)
(load "lib.scm")
(load "p39.scm")

(print
	(fixed-point
		(lambda (x) (+ 1 (/ 1 x)))
		1.0))
; 1.6180327868852458

; END