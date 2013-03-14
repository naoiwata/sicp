;;
;; @author naoiwata
;; SICP Chapter2
;; 2.2.2 Hierarchical Structures
;;

(define (count-leaves x)
	(cond
		((null? x)
			0)
		((not (pair? x))
			1)
		(else
			(+
				(count-leaves (car x))
				(count-leaves (cdr x))))))

; test
(print (count-leaves (list 2 4 8)))
; 3

; END