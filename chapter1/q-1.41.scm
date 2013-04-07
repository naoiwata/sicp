;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.41
;;

(add-load-path "." :relative)
(load "lib.scm")

(define (double f)
  (lambda (x)
    (f (f x))))

(print (((double (double double)) inc) 5))
; ((double f) x)                   -> f(f(x)) = g(x)
; (((double double) f) x)          -> f(f(g(x))) = h(x)
; (((double (double double)) f) x) -> f(f(h(x) + h(x)))
; -> f(f(f(f(g(x))) + f(f(g(x)))))
; -> f(f(f(f(f(f(x)))) + f(f(f(f(x))))))
; -> inc(inc(inc(inc(inc(inc(5)))) + inc(inc(inc(inc(5))))))
; -> inc(10 + 10)
; -> inc(20)
; -> 21

#|
(double inc)
-> (inc (inc x))
|#

; END