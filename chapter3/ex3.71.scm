;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.71.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

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

(define (cube-weight x)
  (let
    ((a (car x))
     (b (cadr x)))
    (+ (* a a a) (* b b b))))

(define (ramanujan-number s)
  (let
    ((s1 (cube-weight (stream-car s)))
     (s2 (cube-weight (stream-car (stream-cdr s)))))
    (if (= s1 s2)
        (cons-stream
          s1
          (ramanujan-number (stream-cdr s)))
        (ramanujan-number (stream-cdr s)))))

(define ramanujan-stream
  (ramanujan-number (weighted-pairs integers integers cube-weight)))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(map
  (lambda (x) (stream-ref ramanujan-stream x))
  (iota 10))
; (1729 4104 13832 20683 32832 39312 40033 46683 64232 65728)

; END