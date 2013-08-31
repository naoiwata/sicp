;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.66.
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

(define factorials
  (cons-stream 1 
               (mul-streams
                 factorials
                 (add-streams ones integers))))

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
; test
; ------------------------------------------------------------------------

(map
  (lambda (n) (stream-ref (pairs integers integers) n))
  (iota 100))

; ((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7) 
; (2 5) (1 8) (4 4) (1 9) (2 6) (1 10) (3 5) (1 11) (2 7) (1 12) (4 5) (1 13)
; (2 8) (1 14) (3 6) (1 15) (2 9) (1 16) (5 5) (1 17) (2 10) (1 18) (3 7) (1 19)
; (2 11) (1 20) (4 6) (1 21) (2 12) (1 22) (3 8) (1 23) (2 13) (1 24) (5 6) (1 25)
; (2 14) (1 26) (3 9) (1 27) (2 15) (1 28) (4 7) (1 29) (2 16) (1 30) (3 10) (1 31)
; (2 17) (1 32) (6 6) (1 33) (2 18) (1 34) (3 11) (1 35) (2 19) (1 36) (4 8) (1 37)
; (2 20) (1 38) (3 12) (1 39) (2 21) (1 40) (5 7) (1 41) (2 22) (1 42) (3 13) (1 43)
; (2 23) (1 44) (4 9) (1 45) (2 24) (1 46) (3 14) (1 47) (2 25) (1 48) (6 7) (1 49)
; (2 26) (1 50) (3 15) (1 51))

; thanks to http://gist.github.com/cocoatomo/6398949

(define (rec-index i j)
  (cond
    ((or (< i 0) (< j 0) (> i j)) -1)
    ((and (= i 0) (= j 0)) 0)
    ((and (= i 0) (> j 0)) (- (* 2 j) 1))
    (else (+ (* 2 (rec-index (- i 1) (- j 1))) 2))))
 
(define (index i j)
  (if (= i j)
      (- (expt 2 (+ i 1)) 2)
      (- (* (expt 2 i) (+ (* 2 (- j i)) 1)) 2)))

(index 1 100)   ; 396
(index 99 100)  ; 1901475900342344102245054808062
(index 100 100) ; 2535301200456458802993406410750

; END