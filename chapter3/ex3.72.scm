;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.72.
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

(define (square-weight x)
  (let
    ((a (car x))
     (b (cadr x)))
    (+ (* a a) (* b b))))

(define (square-sum s)
  (let
    ((s1 (square-weight (stream-car s)))
     (s2 (square-weight (stream-car (stream-cdr s))))
     (s3 (square-weight (stream-car (stream-cdr (stream-cdr s))))))
    (if (= s1 s2 s3)
        (cons-stream
          s1
          (square-sum (stream-cdr s)))
        (square-sum (stream-cdr s)))))

(define square-stream
  (square-sum (weighted-pairs integers integers square-weight)))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(map
  (lambda (x) (stream-ref square-stream x))
  (iota 10))
; (325 425 650 725 845 850 925 1025 1105 1105)

; END