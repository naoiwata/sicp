;; @author naoiwata
;; SICP Chapter2
;; question 2.25

#|
1. (1 3 (5 7) 9)
2. ((7))
3. (1 (2 (3 (4 ( 5 ( 6 7))))))
|#

(define q1 (list 1 3 (list 5 7) 9))
(define q2 (list (list 7)))
(define q3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(print (car (cdr (car (cdr (cdr q1)))))) 
; 7
(print (car (car q2))) 
; 7
(print (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr q3))))))))))))) 
; 7

; END