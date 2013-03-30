;; @author naoiwata
;; SICP Chapter2
;; question 2.70

(add-load-path "." :relative)
(load "pages/2.3.4.scm")
(load "q2.68.scm")
(load "q2.69.scm")

(define rock-tree
  (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define rock-message
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(define code (encode rock-message rock-tree))

(print code)
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
(print (length code))
; 84 

(print (length rock-message)) ; 36
; 36 * 3(log8) = 108. 

; fixed-length    -> 108 bits
; variable-length -> 84 bits 
; so, about 22.2 % is cut down.

; END