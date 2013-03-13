;;
;; @author naoiwata
;; SICP Chapter2
;; question 2.7
;;

(add-load-path "." :relative)
(load "pages/2.1.4.scm")

(define (make-interval a b) 
	(cons a b))

; uppor-bound
(define (upper-bound x)
	(max (car x) (cdr x)))

; lower-bound
(define (lower-bound x)
	(min (car x) (cdr x)))

; END