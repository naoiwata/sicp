;; @author naoiwata
;; SICP Chapter2
;; question 2.38

(add-load-path "." :relative)
(load "pages/lib.scm")

(define (fold-right op initial sequence)
	(if (null? sequence)
		initial
		(op
			(car sequence)
			(fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter 
				(op result (car rest))
				(cdr rest))))
	(iter initial sequence))

; test-1
(print (fold-right / 1 (list 1 2 3))) ; 3/2
; (/ 1 (/ 2 (/ 3 1))) -> 3/2
(print (fold-left / 1 (list 1 2 3))) ; 1/6
; (iter (/ 1 1) (2 3)) = (iter 1 (2 3))
; -> (iter (/ 1 2) (3)) = (iter 1/2 (3))
; (iter (/ 1/2 3) ()) = 1/6

; test-2
(print (fold-right list () (list 1 2 3))) ; (1 (2 (3 ())))
; (list 1 (list 2 (list 3 ())))
(print (fold-left list () (list 1 2 3))) ; (((() 1) 2) 3)
; (iter (list () 1)) (2 3)) = (iter (() 1)) (2 3)) 
; (iter (list (() 1)) 2) (3)) = (iter ((() 1)) 2) (3))
; (iter (list ((() 1)) 2) 3) ()) = (((() 1)) 2) 3)

; どのようなならびに対しても同じ値を生じる為には、opの演算を行う順序が手続きの順序に依存しないという条件を満たす必要がある。
; (例)
(print (fold-right * 2 (list 1 2 3))) ; 12
(print (fold-left * 2 (list 1 2 3))) ; 12

; END