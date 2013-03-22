;; @author naoiwata
;; SICP Chapter2
;; question 2.55

(print (''abracadabra)) 
; ERROR

(print (list ''abracadabra)) 
; ('abracadabra)

(print (car ''abracadabra)) 
; quote

(print (cdr ''abracadabra)) 
; (abracadabra)

(print (car 'abracadabra)) 
; ERROE

; it seems that ''abracadabra returns (list 'quote 'abracadabra)
(print (car (list 'quote 'abracadabra))) ; quote

; END
