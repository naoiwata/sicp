;; @author naoiwata
;; SICP Chapter2
;; question 2.14
;;

(add-load-path "." :relative)
(load "q2.12.scm")

; copy
(define (par1 r1 r2)
	(div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
	(let 
		((one (make-interval 1 1)))
	(div-interval one
		(add-interval 
			(div-interval one r1)
			(div-interval one r2)))))

; solusion
(define x (make-center-percent 1.0 3.0))
(define y (make-center-percent 7.0 8.0))
(define a (par1 x y))
(define b (par2 x y))

(print a) ; (0.7272176949941792 . 0.8536903376018627)
(print b) ; (0.8430229419703104 . 0.9064959254947613)
(print (center a)) ; 0.7904540162980209
(print (center b)) ; 0.8747594337325358
(print (percent a)) ; 8.000000000000012
(print (percent b)) ; 3.628025093345738
; these are diffirent results.

; END