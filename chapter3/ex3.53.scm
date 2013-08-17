;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.53.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define s 
  (cons-stream 1 (add-streams s s)))

; 1, 2, 4, 8, 16....
; f(n) = 2^(n-1)

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print (stream-ref s 0)) ; 1
(print (stream-ref s 1)) ; 2
(print (stream-ref s 2)) ; 4
(print (stream-ref s 3)) ; 8
(print (stream-ref s 4)) ; 16

; END