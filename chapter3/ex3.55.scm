;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.55.
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

(define (partical-sums S)
  (cons-stream (stream-car S)
               (add-streams (stream-cdr S)
                            (partical-sums S))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print (stream-ref (partical-sums integers) 0)) ; 1
(print (stream-ref (partical-sums integers) 1)) ; 3
(print (stream-ref (partical-sums integers) 2)) ; 6
(print (stream-ref (partical-sums integers) 3)) ; 10
(print (stream-ref (partical-sums integers) 4)) ; 15

; END