;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.42
;;

(add-load-path "." :relative)
(load "lib.scm")

(define (compose f g)
	(lambda (x)
		(f (g x))))

(print
	((compose square inc) 6))
; 49

; END