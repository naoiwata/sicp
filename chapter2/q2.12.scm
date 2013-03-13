;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.12
;;

(add-load-path "." :relative)
(load "q2.7.scm")

; copy
(define (make-center-width c w)
	(make-interval (- c w) (+ c w)))

(define (center i)
	(/
		(+ (lower-bound i) (upper-bound i))
		2))

(define (width i)
	(/
		(- (upper-bound i) (lower-bound i))
		2))

; solution
; a--.--A
; -- = w
; .  = (a + A)/2
; n %
; w = (a + w) * n/100
; -> n = (100 * w)/(a + w)
; -> n = (200 * width)/center
(define (percent i)
	(/
		(* 200 (width i))
		(center i)))

; (center - center*n/100) ~ (center + center*n/100)
(define (make-center-percent center percent)
	(let
		((width (* center (/ percent 100))))
		(make-interval
			(- center width)
			(+ center width))))

; END
