;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.70.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (merge-weighted s1 s2 weight)
  (cond 
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let 
        ((s1car (stream-car s1))
         (s2car (stream-car s2)))
        (let
          ((w1 (weight s1car))
           (w2 (weight s2car)))
          (if (< w1 w2)
              (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
              (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

; (a)
(define (weight-sum-a pairs)
  (+ (car pairs) (cadr pairs)))
(define sum-ij-a
        (weighted-pairs integers integers weight-sum-a))

; (b)
(define (weight-sum-b pairs)
  (+ (* 2 (car pairs)) (* 3 (cadr pairs)) (* 5 (car pairs) (cadr pairs))))
(define integers-235
  (stream-filter
    (lambda (x) 
      (and (not (= (remainder x 2) 0))
           (not (= (remainder x 3) 0))
           (not (= (remainder x 5) 0))))
    integers))
(define sum-ij-b
        (weighted-pairs integers-235 integers-235 weight-sum-b))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

; (a)
(map
  (lambda (x) (stream-ref (weighted-pairs integers integers sum-i+j) x))
  (iota 10))
; ((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (2 4) (1 5) (3 4))

; (b)
(map
  (lambda (x) (stream-ref (weighted-pairs integers integers sum-2i+3j+5ij) x))
  (iota 10))
; ((1 1) (1 2) (1 3) (2 2) (1 4) (1 5) (2 3) (1 6) (2 4) (1 7))
