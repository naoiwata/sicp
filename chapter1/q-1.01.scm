;
; @author naoiwata
; SICP Chapter1
; q1.1
;

; 10
(print 10)
; 10

; (+ 5 3 4)
(print (+ 5 3 4))
; 12

; (- 9 1)
(print (- 9 1))
; 8

; (/ 6 2)
(print (/ 6 2))
; 3

; (+ (* 2 4) (- 4 6))
(print (+ (* 2 4) (- 4 6)))
; 6

; (define a 3)
(define a 3)
(print a)
; 3

; (define b (+ a 1))
(print (define b (+ a 1)))
(print b)
; 4

; (= a b)
(print (= a b))
; #f

; (if (and (> b a) (< b (* a b)))
;     b
;     a)
(print (if (and (> b a) (< b (* a b))) b a))
; 4

; (cond ((= a 4) 6)
;		((= b 4) (+ 6 7 a))
;		(else 25))
(print (cond ((= a 4) 6) ((= b 4) (+ 6 7 a)) (else 25)))
; (+ 6 7 3)
; 16

; (+ 2 (if (> b a) b a))
(print (+ 2 (if (> b a) b a)))
; (+ 2 b)
; (+ 2 4)
; 6

; (* (cond ((> a b) a)
;			((< a b) b)
;			(else -1))
;	(+ a 1))
(print  (* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1)))
; (* b (+ a 1))
; (* 4 (+ 3 1))
; (* 4 4)
; 16
