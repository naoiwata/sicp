;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.43
;;

(add-load-path "." :relative)
(load "q-1.42.scm")

(define (repeated f n)
	(if (= n 1)
		(lambda (x) (f x))
		(compose f (repeated f (- n 1)))))

; test
(print
	((repeated square 2) 5))
; 625

; END