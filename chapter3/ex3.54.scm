;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.54.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 
               (mul-streams
                 factorials
                 (add-streams ones integers))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print (stream-ref factorials 0)) ; 1
(print (stream-ref factorials 1)) ; 2
(print (stream-ref factorials 2)) ; 6
(print (stream-ref factorials 3)) ; 24
(print (stream-ref factorials 4)) ; 120

; END