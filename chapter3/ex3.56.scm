;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.56.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond 
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let 
        ((s1car (stream-car s1))
         (s2car (stream-car s2)))
        (cond 
          ((< s1car s2car)
           (cons-stream s1car (merge (stream-cdr s1) s2)))
          ((> s1car s2car)
           (cons-stream s2car (merge s1 (stream-cdr s2))))
          (else
            (cons-stream s1car
                         (merge (stream-cdr s1)
                                (stream-cdr s2)))))))))

(define S (cons-stream 1 
                       (merge (scale-stream S 2)
                              (merge (scale-stream S 3)
                                     (scale-stream S 5)))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print 
  (map
    (lambda (n) (stream-ref S n))
    (iota 100)))
; (1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54 60 64 72 75 80 81 90 96 100 108 120 125 128 135 144 150 160 162 180 192 200 216 225 240 243 250 256 270 288 300 320 324 360 375 384 400 405 432 450 480 486 500 512 540 576 600 625 640 648 675 720 729 750 768 800 810 864 900 960 972 1000 1024 1080 1125 1152 1200 1215 1250 1280 1296 1350 1440 1458 1500 1536)

; END