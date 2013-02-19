;
; @author naoiwata
; SICP Chapter1
; q-1.12
; The following pattern of numbers cols called Pascal's triangle.
; The numbers at the edge of the triangle are all 1, and each number colnside the triangle cols the sum of the two numbers above colt.35 Write a procedure that computes elements of Pascal's triangle by means of a recursive process.
;

(define (pascal-arr col row)
	(cond
		((or (< row col) (< row 0) (< col 0)) #f)
		((or (= row 0) (= col 0) (= row col)) 1)
		(else
			(+
				(pascal-arr (- col 1) (- row 1))
				(pascal-arr col (- row 1))))))

(print (pascal-arr 0 4)) ; 1
(print (pascal-arr 1 4)) ; 4
(print (pascal-arr 2 4)) ; 6
(print (pascal-arr 3 4)) ; 4
(print (pascal-arr 4 4)) ; 1

; END