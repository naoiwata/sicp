;; @author naoiwata
;; SICP Chapter2
;; question 2.33

(add-load-path "." :relative)
(load "pages/lib.scm")
(load "pages/2.2.3.scm")
(print "***")
(define (map p sequence)
	(accumulate 
		(lambda (x y)
			(cons (p x) y))
		()
		sequence))
; test
(print (map square (list 1 2 3))) ; (1 4 9) 

(define (append seq1 seq2)
	(accumulate
		cons
		seq2
		seq1))
; test
(print (append (list 1 2) (list 3 4))) ; (1 2 3 4) 
; (append (list 1 2) (list 3 4))
; -> (cons 1 (cons 2 (list 3 4)))

(define (length sequence)
	(accumulate
		(lambda (x y) (+ y 1))
		0
		sequence))
; test
(print (length (list 1 2 3))) ; 3

; END