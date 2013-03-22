;; @author naoiwata
;; SICP Chapter2
;; question 2.53

(list 'a 'b 'c) 
; (a b c)

(list (list 'george)) 
; (list '(george)) -> ((gerge)) 

(cdr '((x1 x2) (y1 y2))) 
; -> ((y1 y2))

(cadr '((x1 x2) (y1 y2))) 
; (car (cdr '((x1 x2) (y1 y2)))) -> (cdr '((y1 y2))) -> (y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; (memq 'red '((blue socks))) -> #f

(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; END