;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.10
;;

(add-load-path "." :relative)
(load "pages/2.1.4.scm")
(load "q2.7.scm")
(load "q2.8.scm")
(load "q2.9.scm")

; if a < 0 < b -> a*b < 0
(define (div-interval x y)
	(if (< (* (lower-bound y) (upper-bound y)) 0)
		(error "this is an error")
		(mul-interval x
			(make-interval 
				(/ 1.0 (upper-bound y))
				(/ 1.0 (lower-bound y))))))

; test
(print (div-interval (make-interval 0 2) (make-interval -2 2)))
; "error": this is an error
(print (div-interval (make-interval 1 2) (make-interval 4 6)))
;　(0.16666666666666666 . 0.3333333333333333)

; END