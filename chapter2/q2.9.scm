;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.9
;;

(add-load-path "." :relative)
(load "pages/2.1.4.scm")
(load "q2.7.scm")
(load "q2.8.scm")

(define (width-interval x)
	(/
		(- (upper-bound x) (lower-bound x))
		2))

; test
(define R1 (make-interval 3.0 1.0))
(define R2 (make-interval 10.0 9.0))
(define R3 (make-interval 16.0 13.0))

(print (width-interval R1)) ; 1.0
(print (width-interval R2)) ; 0.5

;; add 
(print (add-interval R1 R2)) ; (10.0 . 13.0)
(print (width-interval (add-interval R1 R2))) ; 1.5

;; sub
(print (sub-interval R1 R2)) ; (-8.0 . -7.0)
(print (width-interval (sub-interval R1 R2))) ; 0.5

;; mul
(define (mul-interval x y)
	(make-interval 
		(* (lower-bound x) (lower-bound y))
		(* (upper-bound x) (upper-bound y))))

(print (mul-interval R1 R2)) ; (9.0 . 30.0)
(print (width-interval (mul-interval R1 R2))) ; 10.5
(print (mul-interval R1 R3)) ; (13.0 . 48.0)
(print (width-interval (mul-interval R1 R3))) ; 17.5
; -> these result are not equal..

;; div
(define (div-interval x y)
	(make-interval 
		(/ (lower-bound x) (lower-bound y))
		(/ (upper-bound x) (upper-bound y))))

(print (div-interval R1 R2)) ; (0.1111111111111111 . 0.3)
(print (width-interval (div-interval R1 R2))) ; 0.09444444444444444
(print (mul-interval R1 R3)) ; (13.0 . 48.0)
(print (width-interval (div-interval R1 R3))) ; 0.055288461538461536
; -> these result are not equal..

; END