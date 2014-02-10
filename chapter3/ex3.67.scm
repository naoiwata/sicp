;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.67.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map
          (lambda (x) (list (stream-car s) x))
           (stream-cdr t))
        (stream-map
          (lambda (x) (list (stream-car t) x))
           (stream-cdr s)))
      (pairs (stream-cdr s) (stream-cdr t)))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(map
  (lambda (n) (stream-ref (pairs integers integers) n))
  (iota 100))

; ((1 1) (1 2) (2 2) (2 1) (2 3) (1 3) (3 3) (3 1) (3 2) (1 4) (3 4) (4 1) (2 4) (1 5) (4 4) (5 1) (4 2) (1 6) (4 3) (6 1) (2 5) (1 7) (4 5) (7 1) (5 2) (1 8) (3 5) (8 1) (2 6) (1 9) (5 5) (9 1) (6 2) (1 10) (5 3) (10 1) (2 7) (1 11) (5 4) (11 1) (7 2) (1 12) (3 6) (12 1) (2 8) (1 13) (5 6) (13 1) (8 2) (1 14) (6 3) (14 1) (2 9) (1 15) (4 6) (15 1) (9 2) (1 16) (3 7) (16 1) (2 10) (1 17) (6 6) (17 1) (10 2) (1 18) (7 3) (18 1) (2 11) (1 19) (6 4) (19 1) (11 2) (1 20) (3 8) (20 1) (2 12) (1 21) (6 5) (21 1) (12 2) (1 22) (8 3) (22 1) (2 13) (1 23) (4 7) (23 1) (13 2) (1 24) (3 9) (24 1) (2 14) (1 25) (6 7) (25 1) (14 2) (1 26) (9 3) (26 1))

; END