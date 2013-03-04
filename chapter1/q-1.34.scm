;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.34
;;

(add-load-path "." :relative)
(load "lib.scm")

(define (f g)
	(g 2))

(print
	(f square))
; -> 4

(print
(f (lambda (z) (* z (+ z 1)))))
; -> 6

; discuss about this
; (f f)
; -> ((lambda (g) (g 2)) (lambda (g) (g 2)))
; -> (2 2)

; END