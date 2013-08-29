;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.65.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

; partial-sums
(map 
  (lambda (x) (print (stream-ref ln-stream x))) 
  (iota 20))
; 1.0
; 0.5
; 0.8333333333333333
; 0.5833333333333333
; 0.7833333333333332
; 0.6166666666666666
; 0.7595238095238095
; 0.6345238095238095
; 0.7456349206349207
; 0.6456349206349207
; 0.7365440115440116
; 0.6532106782106782
; 0.7301337551337552
; 0.6587051837051838
; 0.7253718503718505
; 0.6628718503718505
; 0.7216953797836152
; 0.6661398242280596
; 0.718771403175428
; 0.6687714031754279

; euler-transform
(define (euler-transform s)
  (let 
    ((s0 (stream-ref s 0))
     (s1 (stream-ref s 1))
     (s2 (stream-ref s 2)))
    (cons-stream
      (- s2 (/ (square (- s2 s1))
               (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))

(display-stream (euler-transform ln-stream))
; 0.7
; 0.6904761904761905
; 0.6944444444444444
; 0.6924242424242424
; 0.6935897435897436
; 0.6928571428571428
; 0.6933473389355742
; 0.6930033416875522
; 0.6932539682539683
; 0.6930657506744464
; 0.6932106782106783
; 0.6930967180967181
; 0.6931879423258734
; 0.6931137858557215
; 0.6931748806748808
; 0.6931239512121866
; 0.6931668512550866
; 0.6931303775344023
; 0.693161647077867
; 0.6931346368409872

; tableau
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-square transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-stream (accelerated-square euler-transform ln-stream))
; 1.0
; 0.7
; 0.6932773109243697
; 0.6931488693329254
; 0.6931471960735491
; 0.6931471806635636
; 0.6931471805604039
; 0.6931471805599445 <- convergence
; 0.6931471805599427
; 0.6931471805599454
; +nan.0
; +nan.0
; +nan.0
; +nan.0
; +nan.0
; +nan.0
; +nan.0
; +nan.0
; +nan.0
; +nan.0

; END