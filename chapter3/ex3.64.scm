;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.64.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (stream-limit streams tolerance)
  (let 
    ((s1 (stream-car streams))
     (s2 (stream-car (stream-cdr streams))))
    (if (< (abs (- s1 s2)) tolerance)
        s2
        (stream-limit (stream-cdr streams) tolerance))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.000001)
; 1.414213562373095

; END