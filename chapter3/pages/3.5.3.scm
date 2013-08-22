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

(print (map (lambda (x) (print (stream-ref pi-stream x))) (iota 20)))
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

