;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.1
;;

(add-load-path "." :relative)
(load "pages/2.1.1.scm")

(define (make-rat n d)
	(let
		((g (gcd n d))
		(s (if (< d 0) -1 1)))
		(cons
			(* s (/ n g))
			(* s (/ d g)))))

; test
(define (test1) (make-rat -3 1))
(define (test2) (make-rat 1 -3))
(newline)
(print-rat (test1)) 
(print-rat (test2)) ; -1/3

; END