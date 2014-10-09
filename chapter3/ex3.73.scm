;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.73.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (integral (scale-stream i (/ 1 C)) v0 dt)
                 (scale-stream i R))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(define RC1 (RC 5 1 0.5))

(define ones (cons-stream 1 ones))

(define (scale-stream stream factor)
   (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
   (stream-map + s1 s2))

(print (map
  (lambda (x) (stream-ref (RC1 ones 10) x))
  (iota 10)))
;; => (15 15.5 16.0 16.5 17.0 17.5 18.0 18.5 19.0 19.5)
