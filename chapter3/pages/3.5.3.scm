;;
;; @author naoiwata
;; SICP Chapter3
;; 3.5.3 Exploiting the Stream Paradigm
;;

(add-load-path "./" :relative)
(load "stream.scm")

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                   (lambda (guess)
                     (sqrt-improve guess x))
                   guesses)))
  guesses)

(define average
  (lambda (a b)
    (/ (+ a b) 2)))

(print (map (lambda (x) (stream-ref (sqrt-stream 2) x)) (iota 20)))
; 1.0 
; 1.5 
; 1166666666666665 
; 1142156862745097 
; 1142135623746899 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095 
; 114213562373095

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map
                 -
                 (pi-summands (+ n 2)))))

(define (partial-sums S)
  (cons-stream (stream-car S)
               (add-streams (stream-cdr S)
                            (partial-sums S))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(print (map (lambda (x) (print (stream-ref pi-stream x))) (iota 2000)))

; 4.0
; 2.666666666666667
; 3.466666666666667
; 2.8952380952380956
; 3.3396825396825403
; 2.9760461760461765
; 3.2837384837384844
; 3.017071817071818
; 3.2523659347188767
; 3.0418396189294032
; 3.232315809405594
; 3.058402765927333
; 3.2184027659273333
; 3.0702546177791854
; 3.208185652261944
; 3.079153394197428
; 3.200365515409549
; 3.0860798011238346
; 3.1941879092319425
; 3.09162380666784
; ...
; 3.1466687630233094
; 3.1365421807448284
; 3.146617747495458
; 3.136592684838816 <- 200

(define square (lambda (x) (* x x)))

(define (euler-transform s)
  (let 
    ((s0 (stream-ref s 0))
     (s1 (stream-ref s 1))
     (s2 (stream-ref s 2)))
    (cons-stream
      (- s2 (/ (square (- s2 s1))
               (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))
  
(display-stream (euler-transform pi-stream))

; 3.166666666666667
; 3.1333333333333337
; 3.1452380952380956
; 3.13968253968254
; 3.1427128427128435
; 3.1408813408813416
; 3.142071817071818
; 3.1412548236077655
; 3.1418396189294033
; 3.141406718496503
; 3.1417360992606667
; 3.1414796890042562
; 3.1416831892077566
; 3.1415189855952774
; 3.141653394197428
; 3.1415419859977844
; 3.14163535667939
; 3.141556330284574
; 3.14162380666784
; 3.141565734658548
; 3.141616071918188
; 3.141572154482966
; 3.1416106990404744
; 3.1415766854350324
; 3.141606851347551
; 3.141579973962183
; 3.141604023986233
; 3.141582418247749
; 3.1416019000322803
; 3.1415842726746215
; 3.141600273698687
; 3.1415857049341183
; 3.141599007405717

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-square transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-stream (accelerated-square euler-transform pi-stream))

; 4.0
; 3.166666666666667
; 3.142105263157895
; 3.141599357319005
; 3.1415927140337785
; 3.1415926539752927
; 3.1415926535911765
; 3.141592653589778
; 3.1415926535897953
; 3.141592653589795

; ------------------------------------------------------------------------
; Infinite streams of pairs
; ------------------------------------------------------------------------

(stream-filter
  (lambda (pair)
    (prime? (+ (car pair) (cadr pair))))
  (int-pairs))

(stream-map
  (lambda (x)
    (list ((stream-car s) x)))
  (stream-cdr t))

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


