;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.45
;;

(add-load-path "." :relative)
(load "p41.scm")
(load "q-1.43.scm")

(define (n-sqrt x n i)
	(fixed-point
		((repeated average-damp i)
			(lambda (y) 
				(/ x (expt y (- n 1)))))
		1.0))

; test (n = 2)
(print (n-sqrt 100 2 1)) ; 10.0
(print (n-sqrt 100 2 2)) ; 10.000005110362242
(print (n-sqrt 100 2 3)) ; 10.000023399641949

; test (n = 3)
(print (n-sqrt 1000 3 1)) ; 10.000002544054729
(print (n-sqrt 1000 3 2)) ; 10.000001344613787
(print (n-sqrt 1000 3 3)) ; 10.000016556176512

; test (n = 4)
; (print (n-sqrt 10000 4 1)) ; ERROR!
(print (n-sqrt 10000 4 2)) ; 10.0
(print (n-sqrt 10000 4 3)) ; 10.000005682245106

; test (n = 5)
; (print (n-sqrt 100000 5 1)) ; ERROR!
(print (n-sqrt 100000 5 2)) ; 9.99999869212542
(print (n-sqrt 100000 5 3)) ; 10.000003715932646

; test (n = 6)
(print (n-sqrt 1000000 6 2)) ; 9.999996858149522
(print (n-sqrt 1000000 6 3)) ; 10.000001327778605
(print (n-sqrt 1000000 6 4)) ; 10.00001658526328

; test (n = 8)
; (print (n-sqrt 100000000 8 2)) ; ERROR!
(print (n-sqrt 100000000 8 3)) ; 10.0
(print (n-sqrt 100000000 8 4)) ; 10.00000883952346

; test (n = 16)
(print (n-sqrt 10000000000000000 16 4)) ; 10.0
(print (n-sqrt 10000000000000000 16 5)) ; 10.000008503182556

; results
; i : required average damps times 
; n | 1 | 2 | 3 | 4 | 5 |  6  | ... | 8 | ... | 16 |
; i | / | 1 | 1 | 2 | 2 | 2~3 | ... | 3 | ... |  4 | 
; i(n) = log(n)/log(2)

; so, this is written such an expression,
(define (n-sqrt-new x n)
	(let 
		((i (round (/ (log n) (log 2)))))
		(fixed-point
			((repeated average-damp i)
				(lambda (y) 
					(/ x (expt y (- n 1)))))
			1.0)))

; test
(print (n-sqrt-new 100000000 8))
; 10.0

; END