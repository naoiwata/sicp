;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.69.
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

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map
        (lambda (x) (cons (stream-car s) x))
        (pairs t u))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define triples-integers
  (triples integers integers integers))

(define square (lambda (x) (* x x)))

(define pythagoras
  (stream-filter
    (lambda (n)
      (= (+ (square (car n)) (square (cadr n))) (square (caddr n))))
    triples-integers))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(map
  (lambda (n) (stream-ref (triples integers integers integers) n))
  (iota 100))
; ((1 1 1) (1 1 1) (2 2 2) (1 1 2) (2 2 2) (1 2 2) (3 3 3) (1 1 3) (2 2 3) (1 2 3) (3 3 3) (1 1 4) (2 3 3) (1 3 3) (4 4 4) (1 1 5) (2 2 4) (1 2 4) (3 3 4) (1 1 6) (2 3 4) (1 3 4) (4 4 4) (1 1 7) (2 2 5) (1 2 5) (3 4 4) (1 1 8) (2 4 4) (1 4 4) (5 5 5) (1 1 9) (2 2 6) (1 2 6) (3 3 5) (1 1 10) (2 3 5) (1 3 5) (4 4 5) (1 1 11) (2 2 7) (1 2 7) (3 4 5) (1 1 12) (2 4 5) (1 4 5) (5 5 5) (1 1 13) (2 2 8) (1 2 8) (3 3 6) (1 1 14) (2 3 6) (1 3 6) (4 5 5) (1 1 15) (2 2 9) (1 2 9) (3 5 5) (1 1 16) (2 5 5) (1 5 5) (6 6 6) (1 1 17) (2 2 10) (1 2 10) (3 3 7) (1 1 18) (2 3 7) (1 3 7) (4 4 6) (1 1 19) (2 2 11) (1 2 11) (3 4 6) (1 1 20) (2 4 6) (1 4 6) (5 5 6) (1 1 21) (2 2 12) (1 2 12) (3 3 8) (1 1 22) (2 3 8) (1 3 8) (4 5 6) (1 1 23) (2 2 13) (1 2 13) (3 5 6) (1 1 24) (2 5 6) (1 5 6) (6 6 6) (1 1 25) (2 2 14) (1 2 14) (3 3 9) (1 1 26))

(map
  (lambda (n) (stream-ref pythagoras n))
  (iota 4))
; ((3 4 5) (6 8 10) (5 12 13) (9 12 15))

; END