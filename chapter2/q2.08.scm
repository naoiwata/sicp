;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.8
;;

(define (sub-interval x y)
	(make-interval 
		(- (lower-bound x) (lower-bound y))
		(- (upper-bound x) (upper-bound y))))

; END