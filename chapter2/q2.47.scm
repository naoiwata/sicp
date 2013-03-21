;; @author naoiwata
;; SICP Chapter2
;; question 2.47

; selector 1
(define (make-frame origin edge1 edge2)
	(list origin edge1 edge2))

(define origin
	(car make-frame))

(define edge1
	(car (cdr make-frame)))

(define edge2
	(car (cdr (cdr make-frame))))

; selector 2
(define (make-frame origin edge1 edge2)
	(cons origin (cons edge1 edge2)))

(define origin
	(car make-frame))

(define edge1
	(car (cdr make-frame)))

(define edge2
	(cdr (cdr make-frame)))

; END