;; @author naoiwata
;; SICP Chapter2
;; question 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(print (append x y))
; -> (1 2 3 4 5 6)

(print (cons x y))
; -> ((1 2 3) 4 5 6)

(print (list x y))
; -> ((1 2 3) (4 5 6))

; END