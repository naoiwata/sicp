;; @author naoiwata
;; SICP Chapter2
;; question 2.36

(add-load-path "." :relative)
(load "pages/2.2.3.scm")

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		()
		(cons 
			(accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))
; test
(print (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))))
;(22 26 30)

; END