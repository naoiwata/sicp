;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.38
;;

(add-load-path "." :relative)
(load "q-1.37.scm")

; e = lim (1 + 1/n)^n (n -> infinity)
(define e
		(+
			2
			(cont-franc
				(lambda (i) 1.0)
				(lambda (i)
					(if
						(= (remainder (+ i 1) 3) 0)
							(* (+ i 1) (/ 2 3))
							1))
			100)))
(print e)
; 2.7182818284590455

; END