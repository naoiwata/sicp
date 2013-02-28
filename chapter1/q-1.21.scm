;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.21
;;

(add-load-path "." :relative)
(load "p28-1.scm")

(print
	(smallest-divisor 199)    ; 199
	(smallest-divisor 1999)   ; 1999
	(smallest-divisor 19999)) ; 7

; END