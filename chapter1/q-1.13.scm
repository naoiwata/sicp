;
; @author naoiwata
; SICP Chapter1
; q-1.13
; Prove that Fib(n) is the closest integer to n1/ √5, where = (1 + √5)/2. 
; Hint: Let n2 = (1 - √5)/2. Use induction and the definition of the Fibonacci numbers (see section 1.2.2) to prove that Fib(n) = ( n1 - n2)/ √5.
; 

(define Fib-a
	(/ (+ 1 (sqrt 5)) 2))

(define Fib-b
	(/ (- 1 (sqrt 5)) 2))

(define (math-pow n m)
	(if (= m 0)
		1
		(* n (math-pow n (- m 1)))))

(define (Fib-n n)
	(/ (- (math-pow Fib-a n) (math-pow Fib-b n)) (sqrt 5)))

; (true? (< (abs (- (Fib-n n)) (/ (math-pow Fib-a n) (sqrt 5)) (/ 1 2)))
;; see naoiwata.blogspot.com
(print Fib-b)
(print (/ 2 (sqrt 5)))

; END