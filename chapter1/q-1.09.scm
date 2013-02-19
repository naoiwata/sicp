;
; -> @author naoiwata
; -> SICP Chapter1
; -> q-1.9
;

(define (inc n)
	(+ n 1))

(define (dec n)
	(- n 1))

;(A)
(define (pulsA a b)
	(if (= a 0)
		b
		(inc (+ (dec a) b))))

(pulsA 4 5)
; -> (inc (+ (dec 4) 5))
; -> (inc (inc (+ (dec 3) 5)))
; -> (inc (inc (inc (+ (dec 2) 5))))
; -> (inc (inc (inc (inc (+ (dec 1) 5)))))
; -> (inc (inc (inc (inc (inc (+ 0) 5)))))
; -> (inc (inc (inc (inc 5))))
; -> (inc (inc (inc 6)))
; -> (inc (inc 7))
; -> (inc 8)
; ->  9
; -> 
; recursive process

;(B)
(define (pulsB a b)
	(if (= a 0)
		b
		(+ (dec a) (inc b))))

(pulsB 4 5)
; -> (+ (dec 4) (inc 5))
; -> (+ (dec 3) (inc 6))
; -> (+ (dec 2) (inc 7))
; -> (+ (dec 1) (inc 8))
; -> 9
; -> 
; iterative process
