;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.40
;;

(add-load-path "." :relative)
(load "p42.scm")

(define (cubic a b c)
	(lambda (x)
		(+
			(cube x)
			(* a (square x))
			(* b x)
			c)))

(newton (cubic a b c) 1.0)

; END