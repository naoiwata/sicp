;
; -> @author naoiwata
; -> SICP Chapter1
; -> q-1.11
; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.
;

; recursive method
(define (rec-f n)
	(if (<= n 3)
		n
		(+ 
			(rec-f (- n 1)) 
			(* 2 (rec-f (- n 2))) 
			(* 3 (rec-f (- n 3))))))

(print (rec-f 10))
; 1657

; iterative method
(define (ite-f n)
	(define (ite-f-iter 0 1 2 0 n)))

(define (ite-f-iter p1 p2 p3 counter n)
	(cond
		((<= n 3) 3)
		((< n counter) p3)
		(else 
			(ite-f-iter 
				p2 
				p3 
				(+ 
					p1
					(* 2 p2)
					(* 3 p3)) 
				(+ counter 1) 
				(- n 1)))))

(print (rec-f 10))
; 1657

; END