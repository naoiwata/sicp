;;
; @author naoiwata
; SICP Chapter2
; question 2.6
;;

; 0
(define zero
	(lambda (f)
		(lambda (x) x)))
; -> λf.λx. x

; +1
(define (add-1 n)
	(lambda (f)
		(lambda (x)
			(f ((n f) x)))))
; -> λf.λx f ((n f) x))

#|
; ---------------- ONE -------------------- ;

; 1 = 0 + 1
(add-1 zero)

(lambda (f) 
	(lambda (x) 
		(f ((zero f) x))))
; -> λf.λx ((zero f) x)

(lambda (f) 
	(lambda (x) 
		(f 
			(((lambda (f)
				(lambda (x) x)) 
			f) 
		x))))
; -> λf.λx (((λf.λx. x) f) x)

(lambda (f) 
	(lambda (x) 
		(f 
			(((lambda (f) f)
		x))))
; -> λf.λx (((λf f) x)

(lambda (f) 
	(lambda (x) 
		(f x)))
; -> λf.λx (f x)
|#

;; one 
(define one
	(lambda (f) 
	 	(lambda (x) 
 			(f x))))
#|
; ----------------- TWO ------------------- ;

; 2 = one +1
(add-1 one)

(lambda (f) 
	(lambda (x) 
		(f 
			((one f) x))))
; -> λf.λx ((one f) x)

(lambda (f) 
	(lambda (x) 
		(f 
			(((lambda (f) 
 			(lambda (x) 
					(f x))) f) x))))
; -> λf.λx (((λf.λx (f x)) f) x)

(lambda (f) 
	(lambda (x) 
		(f 
			(((f x) f) x))))
; -> λf.λx (((f x) f) x)

(lambda (f) 
	(lambda (x) 
		(f 
			(f x))))
; -> λf.λx (f x)

|#

;; two
(define two
	(lambda (f) 
		(lambda (x) 
			(f 
				(f x)))))

; test
(define (inc n)
	(+ n 1))

(print ((one inc) 0))
; 1

(print ((two inc) 0))
; 2

; END