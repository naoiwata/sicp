;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.68.
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
; Louis
; ------------------------------------------------------------------------

(define (pairs s t)
  (interleave
    (stream-map 
      (lambda (x) (list (stream-car s) x)) 
      t)
    (pairs (stream-cdr s) (stream-cdr t))))

; 処理が終わらない. (cdr t) ではなく t を写像しているので遅延評価できず (car t) が強制的に評価対象となり処理が実行され続ける.
